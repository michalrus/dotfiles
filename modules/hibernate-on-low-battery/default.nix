{ config, lib, pkgs, ... }:

let

  name = "hibernate-on-low-battery";
  criticalPercent = 5; # %

in

{

  systemd.services."${name}" = {
    # This should also be run immediately after resume, as the machine
    # will be resumed from sleep for the kernel to hibernate it, if
    # the batter gets very low.
    wantedBy = [ "suspend.target" ];
    after    = [ "suspend.target" ];
    path = with pkgs; [ acpi gawk ];
    serviceConfig = {
      Type = "oneshot";
      # <https://wiki.archlinux.org/index.php/laptop#Hibernate_on_low_battery_level>
      ExecStart = let exec = pkgs.writeScriptBin name ''
        #! ${pkgs.stdenv.shell}
        line=$(acpi -b | grep -F "Battery 0" | awk -F'[,:%]' '{print $2, $3}')
        [ -n "$line" ] || exit 1
        echo "$line" | {
          read -r status capacity
          if [ "$status" = Discharging -a "$capacity" -le ${toString criticalPercent} ]; then
            echo >&2 "Critical battery level: $capacity% ≤ ${toString criticalPercent}%, hibernating."
            ${pkgs.util-linux}/bin/swapon -a
            systemctl hibernate
          fi
        }
      ''; in "${exec}/bin/${name}";
    };
  };

  # But also be triggered periodically:
  systemd.timers."${name}" = {
    partOf = [ "${name}.service" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "*:0/3"; # … every 3 min.
  };

}
