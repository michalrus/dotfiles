{ config, lib, pkgs, ... }:

# This will monitor Internet connectivity (over a few predefined IPs in
# parallel, bypassing the VPN), and restart the modem if none is detected.

let
  user = "connmon";
  uid = 988;
  table = 2964;
  ipsToTest = [
    "8.8.8.8"
    "8.8.4.4"
    "1.1.1.1"
    "1.0.0.1"
    "54.229.14.161"
  ];
  pingWaitSec = 10;
  routerIP = "10.77.3.1";
  routesToCopyNoVPN = [ "default" "10.77.2.0/24" ];

  modem-restart = let
    original = pkgs.fetchFromGitHub {
      owner = "mkorz"; repo = "b618reboot";
      rev = "b6d7d5b877e9d530424df9cde01e3e34c9fcbf82";
      hash = "sha256-cZyLs6EKiWJ2r+ky7TTYXyikF+HNGnlurzYQpEpG75c=";
    };
    patched = pkgs.runCommandNoCC "${original.name}-patched" {} ''
      cp -r ${original} $out
      chmod -R +w $out
      cd $out
      patch -p1 -i ${./huawei-scram-reboot.diff}
    '';
  in pkgs.writeShellApplication {
    name = "modem-restart";
    runtimeInputs = [ (pkgs.python3.withPackages (ps: with ps; [ requests ])) ];
    text = ''
      set -euo pipefail
      export PASSWORD_FILE=${config.age.secrets.hardware_modem_password.path}
      export ROUTER_ADDR=${routerIP}
      # no GC: original=${original}
      exec python3 ${patched}/reboot_router.py
    '';
    derivationArgs.meta.description = "Takes a screenshot on Hyprland (has window selection)";
  };

in

{
  age.secrets.hardware_modem_password = {
    file = ../../../../secrets/hardware_modem_password.age;
    owner = user;
    group = user;
  };

  users.users.${user} = {
    isSystemUser = true;
    group = user;
    home = "/run/${user}";
    inherit uid;
  };
  users.groups.${user}.gid = uid;

  systemd.services."connmon" = {
    description = "Internet connectivity monitor";
    wantedBy = [ "multi-user.target" ];
    after = [ "network-pre.target" "ip-route-novpn-table.service" ];
    wants = [ "network.target" "ip-route-novpn-table.service" ];
    before = [ "network.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
      RuntimeDirectory = user;
      PermissionsStartOnly = true;
      User = user;
      Group = user;
    };
    unitConfig = {
      StartLimitIntervalSec = 0; # no restart rate limiting
    };
    path = with pkgs; [ iproute2 iputils stdenv.shellPackage parallel ];
    # Bypass VPN:
    preStart = ''
      ip rule add uidrange ${toString uid}-${toString uid} lookup ${toString table} priority 2000
    '';
    script = ''
      set -euo pipefail
      # ${lib.getExe pkgs.traceroute} -n 1.1.1.1 || true
      export SHELL=${pkgs.stdenv.shell}
      mkdir -p ~/.parallel/ && touch ~/.parallel/will-cite
      report_next_success=
      while true ; do
        sleep ${toString pingWaitSec}
        if ! parallel --halt now,success=1 --jobs 16 --delay 1 --shuf \
               'ping -c1 -W${toString pingWaitSec} {}' \
               ::: ${lib.escapeShellArgs ipsToTest} >/dev/null 2>/dev/null ; then
          echo "No connectivity with the Internet (no ICMP ping answer from {"${lib.escapeShellArgs ipsToTest}"} in ${toString pingWaitSec} seconds). Restarting the router..."
          report_next_success=1
          if ${lib.getExe modem-restart} ; then
            echo "Modem restart requested successfully. It usually takes it 8 seconds to start rebooting, and then another 40 s before it's back."
            sleep 50  # minimum
          else
            echo "Modem restart failed. Will try again soon."
          fi
        elif [ -n "$report_next_success" ] ; then
          echo "We're back up."
          report_next_success=
        fi
      done
    '';
    postStop = ''
      ip rule del uidrange ${toString uid}-${toString uid} lookup ${toString table} priority 2000 || true
    '';
  };

  # This is needed to keep the no-VPN routing ${table} always up to date:
  systemd.services."ip-route-novpn-table" = let
    copyRoutes = ''
      for target in ${lib.escapeShellArgs routesToCopyNoVPN} ; do
        new_route=$(ip route show "$target" || true)
        if [ -z "$new_route" ] ; then
          ip route delete "$target" table ${toString table} || true
        else
          ip route replace $new_route table ${toString table} || true
        fi
      done
    '';
  in {
    wantedBy = [ "multi-user.target" ];
    after = [ "network-pre.target" ];
    wants = [ "network.target" ];
    before = [ "network.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
    };
    unitConfig = {
      StartLimitIntervalSec = 0; # no restart rate limiting
    };
    path = with pkgs; [ iproute2 ];
    preStart = ''
      set -euo pipefail
      ${copyRoutes}
    '';
    script = ''
      set -euo pipefail
      ip monitor route | while IFS= read -r ; do
        ${copyRoutes}
      done
    '';
    postStop = ''
      ip route flush table ${toString table} || true
    '';
  };

  # Manual `modem-restart` command for all users (preventing Ctrl+C):
  environment.systemPackages = [
    (pkgs.writeShellApplication {
       name = "modem-restart";
       runtimeInputs = [];
       text = ''
         exec sudo ${config.systemd.package}/bin/systemctl start modem-restart
       '';
     })
  ];
  security.sudo = {
    enable = true;
    extraConfig = ''
      %users ALL = (root) NOPASSWD: ${config.systemd.package}/bin/systemctl start modem-restart
    '';
  };
  systemd.services."modem-restart" = {
    serviceConfig = {
      Type = "oneshot";
      ExecStart = lib.getExe modem-restart;
    };
  };
}
