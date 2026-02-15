{
  config,
  lib,
  pkgs,
  ...
}: let
  vsp = "airvpn";
  iface = "wg-${vsp}";
  stateDir = "/var/lib/${vsp}";
  ourInternalIP = "10.166.79.196";
  theirInternalIP = "10.128.0.1";
  pubkey = "PyLCXAQT8KkM4T+dUsOQfn+Ub3pGxfGlxkIApuig+hk=";

  # This routing table routes through AirVPN – used by qBittorrent:
  routingTable = 2968;

  switchServer = pkgs.writeShellApplication {
    name = "vpn-switch-server";
    runtimeInputs = with pkgs; [jq config.systemd.package];
    text = ''
      set -euo pipefail

      candidate=$(jq <${stateDir}/recommended.json --arg name "$1" --arg pubkey ${lib.escapeShellArg pubkey} '
        .servers[] | select(.public_name == $name) | { name: .public_name, ip: .ip_v4_in3, $pubkey }
      ')
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

  listRecommended = pkgs.writeShellScript "listRecommended" ''
    set -euo pipefail
    best=$(jq <${stateDir}/recommended.json -r '
             .continents[] | select(.public_name == "Europe") | .server_best')
    exec jq <${stateDir}/recommended.json --arg best "$best" '
      ([.servers[] | select(.public_name == $best)]
       +
       ([.servers[]
         | select(.health == "ok")
         | select(.continent == "Europe")
         | select(.public_name != $best)
        ] | sort_by(.currentload)
       )
       +
       ([.servers[]
         | select(.health == "ok")
         | select(.continent != "Europe")
         | select(.public_name != $best)
        ] | sort_by(.currentload)
       )
      )'
  '';
in {
  age.secrets.wireguard_airvpn_private_key = {
    file = ../../../../secrets/wireguard_airvpn_private_key.age;
  };

  age.secrets.wireguard_airvpn_preshared_key = {
    file = ../../../../secrets/wireguard_airvpn_preshared_key.age;
  };

  age.secrets.wireguard_airvpn_api_key = {
    file = ../../../../secrets/wireguard_airvpn_api_key.age;
  };

  systemd.services.${vsp} = {
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
      ip address add ${ourInternalIP}/32 dev ${iface}
      wg set ${iface} \
        private-key ${config.age.secrets.wireguard_airvpn_private_key.path} \
        fwmark 51820

      # Use a lower MTU, because we run WireGuard-inside-WireGuard, and
      # the default of 1420 breaks large packets like those of SSH or HTTPS:
      ip link set dev ${iface} mtu 1320

      ip link set up dev ${iface}

      wg set ${iface} \
        peer "$server_pubkey" \
        endpoint "$server_ip":1637 \
        preshared-key ${config.age.secrets.wireguard_airvpn_preshared_key.path} \
        persistent-keepalive 15 \
        allowed-ips 0.0.0.0/0,::/0

      ip route replace ${theirInternalIP} dev ${iface} proto kernel scope link src ${ourInternalIP}

      ip route replace default dev ${iface} table ${toString routingTable}
      ip rule add uidrange ${toString config.users.users.qbittorrent.uid}-${toString config.users.users.qbittorrent.uid} lookup ${toString routingTable} priority 2002
      ip rule add uidrange ${toString config.users.users.cardano.uid}-${toString config.users.users.cardano.uid} lookup ${toString routingTable} priority 2002

      # And a watchdog – on failure, it will cycle through servers, until one works:
      export SHELL=${pkgs.stdenv.shell}
      mkdir -p ~/.parallel/ && touch ~/.parallel/will-cite
      while true ; do
        sleep 10
        # Single ping is not enough for AirVPN, let’s run 4:
        if ! parallel --halt now,success=1 --jobs 16 --delay 1 --shuf \
               'ping -c1 -W10 {}' \
               ::: ${lib.escapeShellArgs (lib.replicate 4 theirInternalIP)} >/dev/null 2>/dev/null ; then
          echo "Failed to reach ${theirInternalIP} through ${iface}, will try the next server..."
          ith=$(( 1 + $(cat ${stateDir}/failure-mode-ith 2>/dev/null || echo -1) ))
          echo "$ith" >${stateDir}/failure-mode-ith
          selected=$(${listRecommended} | jq -r --argjson ith "$ith" '.[$ith].public_name')
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

      ip route del ${theirInternalIP} dev ${iface} || true

      ip route flush table ${toString routingTable} || true
      ip rule del uidrange ${toString config.users.users.qbittorrent.uid}-${toString config.users.users.qbittorrent.uid} lookup ${toString routingTable} priority 2002 || true
      ip rule del uidrange ${toString config.users.users.cardano.uid}-${toString config.users.users.cardano.uid} lookup ${toString routingTable} priority 2002 || true

      ip link del dev ${iface} || true
    '';
  };

  systemd.tmpfiles.rules = [
    "d ${stateDir} 0755 root root -"
  ];

  # ---------------------------------- periodically refresh config ---------------------------------- #

  systemd.timers."${vsp}-refresh-config" = {
    partOf = ["${vsp}-refresh-config.service"];
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "hourly";
      RandomizedDelaySec = "10m";
    };
  };

  systemd.services."${vsp}-refresh-config" = {
    path = with pkgs; [curl jq];
    serviceConfig = {
      Type = "oneshot";
    };
    script = ''
      set -euo pipefail
      cd ${stateDir}
      curl -fsSL -H @${config.age.secrets.wireguard_airvpn_api_key.path} 'https://airvpn.org/api/status/' -o recommended.json.new
      num_servers="$(jq '.servers | length' recommended.json.new)"
      if [ "$num_servers" -ge 64 ] ; then
        mv recommended.json.new recommended.json
        chmod 644 recommended.json
      else
        echo >&2 "Got too little AirVPN servers from their API (only $num_servers), will keep the previous list."
        exit 1
      fi
    '';
  };

  # ---------------------------------- allow users to change servers ---------------------------------- #

  environment.systemPackages = [
    (pkgs.writeShellApplication {
      name = "vpn-change-server-for-torrents";
      runtimeInputs = with pkgs; [coreutils util-linux jq skim];
      text = ''
        set -euo pipefail

        selected=$(${listRecommended} | jq -r '
            .[]
            | "\(.public_name)\t\(.country_name)\t\(.location)\tload: \(.currentload)%"' \
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

  systemd.timers."${vsp}-select-best" = {
    partOf = ["${vsp}-select-best.service"];
    wantedBy = ["timers.target"];
    timerConfig = {
      OnCalendar = "*-*-* 03:45:00";
      RandomizedDelaySec = "30m";
    };
  };

  systemd.services."${vsp}-select-best" = {
    path = with pkgs; [jq];
    serviceConfig = {
      Type = "oneshot";
    };
    script = ''
      set -euo pipefail
      selected=$(${listRecommended} | jq -r '.[0].public_name')
      exec ${lib.getExe switchServer} "$selected"
    '';
  };
}
