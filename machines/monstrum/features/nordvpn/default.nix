{
  config,
  lib,
  pkgs,
  ...
}: let
  vsp = "nordvpn";
  iface = "wg-${vsp}";
  stateDir = "/var/lib/${vsp}";
  ourInternalIP = "10.5.0.2";
  theirInternalIP = "10.5.0.1";
  pingWaitSec = 10;
  numPings = 4; # before considering the connection dead

  switchServer = pkgs.writeShellApplication {
    name = "vpn-switch-server";
    runtimeInputs = with pkgs; [jq config.systemd.package];
    text = ''
      set -euo pipefail
      candidate=$(jq <${stateDir}/recommended.json --arg name "$1" '
        .[]
        | select(.name == $name)
        | { name: .name,
            ip: .station,
            pubkey: ((.technologies[] | select(.identifier == "wireguard_udp")).metadata[] | select(.name == "public_key")).value
          }')
      if [ -n "$candidate" ] ; then
        cat >${stateDir}/selected.json <<<"$candidate"
        service=${vsp}.service
        echo >&2 "Restarting $service..."
        systemctl restart --no-block $service
      else
        echo >&2 "fatal: no such server: '$1'"
        exit 1
      fi
    '';
  };
in {
  age.secrets.wireguard_nordvpn = {
    file = ../../../../secrets/wireguard_nordvpn.age;
  };

  systemd = {
    services = {
      ${vsp} = {
        wantedBy = ["multi-user.target"];
        after = ["network-pre.target"];
        wants = ["network.target"];
        before = ["network.target"];
        serviceConfig = {
          Type = "simple";
          RemainAfterExit = true;
          Restart = "always";
          RestartSec = 5;
          RuntimeDirectory = vsp;
          RuntimeDirectoryMode = "0700";
        };
        unitConfig = {
          StartLimitIntervalSec = 0; # no restart rate limiting
        };
        path = with pkgs; [kmod iproute2 procps wireguard-tools jq iputils parallel];
        script = ''
          set -euo pipefail

          server_json=$(cat ${stateDir}/selected.json)
          jq -r .name   <<<"$server_json" >/run/${vsp}/server_name   &&   server_name=$(cat /run/${vsp}/server_name)
          jq -r .ip     <<<"$server_json" >/run/${vsp}/server_ip     &&     server_ip=$(cat /run/${vsp}/server_ip)
          jq -r .pubkey <<<"$server_json" >/run/${vsp}/server_pubkey && server_pubkey=$(cat /run/${vsp}/server_pubkey)
          unset server_json

          echo "Setting up link with '$server_name'..."

          modprobe wireguard || true
          ip link add dev ${iface} type wireguard
          ip address add ${ourInternalIP}/24 dev ${iface}
          wg set ${iface} \
            private-key ${config.age.secrets.wireguard_nordvpn.path} \
            fwmark 51820

          ip link set up dev ${iface}

          wg set ${iface} \
            peer "$server_pubkey" \
            endpoint "$server_ip":51820 \
            persistent-keepalive 15 \
            allowed-ips 0.0.0.0/0,::/0

          # First, avoid routing loops:
          ip route replace "$server_ip" $(ip route show default | sed -r 's/^default//')

          # And then set up the new narrower routes covering the `default`:
          ip route replace   0.0.0.0/1 dev ${iface}
          ip route replace 128.0.0.0/1 dev ${iface}

          # And a watchdog – on failure, it will cycle through servers, until one works:
          export SHELL=${pkgs.stdenv.shell}
          mkdir -p ~/.parallel/ && touch ~/.parallel/will-cite
          while true ; do
            sleep ${toString pingWaitSec}
            # Single ping is not enough and too wobbly, let’s run a few:
            if ! parallel --halt now,success=1 --jobs 16 --delay 1 --shuf \
                   'ping -c1 -W${toString pingWaitSec} {}' \
                   ::: ${lib.escapeShellArgs (lib.replicate numPings theirInternalIP)} >/dev/null 2>/dev/null ; then
              echo "Failed to reach ${theirInternalIP} through ${iface}, will try the next server..."
              ith=$(( 1 + $(cat ${stateDir}/failure-mode-ith 2>/dev/null || echo -1) ))
              echo "$ith" >${stateDir}/failure-mode-ith
              selected=$(jq -r --argjson ith "$ith" '.[$ith].name' ${stateDir}/recommended.json)
              ${lib.getExe switchServer} "$selected"
              exit 1
            else
              if [ -e ${stateDir}/failure-mode-ith ] ; then
                rm ${stateDir}/failure-mode-ith
              fi
            fi
          done
        '';

        postStop = ''
          set -euo pipefail

          ip link del dev ${iface}

          ip route delete   0.0.0.0/1 || true
          ip route delete 128.0.0.0/1 || true

          server_ip="$(cat /run/${vsp}/server_ip)"
          ip route delete "$server_ip" || true
        '';
      };

      "${vsp}-refresh-config" = {
        path = with pkgs; [curl jq];
        serviceConfig = {
          Type = "oneshot";
        };
        script = ''
          set -euo pipefail
          cd ${stateDir}
          curl -fsSL 'https://api.nordvpn.com/v1/servers/recommendations?limit=999999' -o recommended.json.new
          num_servers="$(jq 'length' recommended.json.new)"
          if [ "$num_servers" -ge 500 ] ; then
            mv recommended.json.new recommended.json
            chmod 644 recommended.json
          else
            echo >&2 "Got too little NordVPN servers from their API (only $num_servers), will keep the previous list."
            exit 1
          fi
        '';
      };

      "${vsp}-select-best" = let
        # XXX: Don’t select the 0th best, because they’re most often slower than, say, 4th best.
        nthBest = 4;
      in {
        path = with pkgs; [jq];
        serviceConfig = {
          Type = "oneshot";
        };
        script = ''
          set -euo pipefail
          selected=$(jq -r '.[${toString nthBest}].name' ${stateDir}/recommended.json)
          exec ${lib.getExe switchServer} "$selected"
        '';
      };
    };

    tmpfiles.rules = [
      "d ${stateDir} 0755 root root -"
    ];

    # ---------------------------------- periodically refresh config ---------------------------------- #

    timers = {
      "${vsp}-refresh-config" = {
        partOf = ["${vsp}-refresh-config.service"];
        wantedBy = ["timers.target"];
        timerConfig = {
          OnCalendar = "hourly";
          RandomizedDelaySec = "10m";
        };
      };

      "${vsp}-select-best" = {
        partOf = ["${vsp}-select-best.service"];
        wantedBy = ["timers.target"];
        timerConfig = {
          OnCalendar = "*-*-* 03:45:00";
          RandomizedDelaySec = "30m";
        };
      };
    };
  };

  # ---------------------------------- allow users to change servers ---------------------------------- #

  environment.systemPackages = [
    (pkgs.writeShellApplication {
      name = "vpn-change-server";
      runtimeInputs = with pkgs; [coreutils util-linux jq skim];
      text = ''
        set -euo pipefail
        selected=$(jq <${stateDir}/recommended.json -r '
            .[]
            | select(.status == "online")
            | select(any(.technologies[]?; .identifier == "wireguard_udp"))
            | "\(.name)\t\(.locations[0].country.city.name)\t\(.load) users"
          ' \
          | column -t -s $'\t' \
          | sk --no-sort --prompt 'server (last is best): ' \
          | sed -r 's/  .*//g')
        exec sudo ${lib.getExe switchServer} "$selected"
      '';
    })
  ];

  security.sudo = {
    enable = true;
    extraConfig = ''
      %users ALL = (root) NOPASSWD: ${lib.getExe switchServer} *
    '';
  };

  # ---------------------------------- jump to the best server at night ---------------------------------- #
}
