{ config, pkgs, lib, ... }:

{

  systemd.services.daily-reboot = {
    script = "exec reboot";
  };

  systemd.timers.daily-reboot = {
    partOf = [ "daily-reboot.service" ];
    wantedBy = [ "timers.target" ];
    timerConfig.OnCalendar = "03:00";
  };

}
