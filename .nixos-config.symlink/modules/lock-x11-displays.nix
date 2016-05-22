{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.lockX11Displays;
in

{
  options = {
    services.lockX11Displays = {
      enable = mkOption {
        type = types.bool;
        default = false;
      };
    };
  };

  config = mkIf cfg.enable {

    systemd.services."lock-x11-displays" = {
      description = "Lock all X11 displays using i3lock (not showing notifications)";
      # This can’t be done in powerManagement.powerDownCommands, because
      # its systemd unit is of the “oneshot” type; we need “forking”.
      # Besides, this service should be used in your WM config — DRY!
      serviceConfig.Type = "forking";
      wantedBy = [ "sleep.target" ];
      before = [ "sleep.target" ];
      path = with pkgs; [ procps bash i3lock ];
      script = ''
        pgrep -f xsession | while read p ; do
          printf '%s %s\n' \
            $(cat /proc/$p/environ | tr '\0' '\n' | grep ^DISPLAY | cut -d = -f 2) \
            $(cat /proc/$p/environ | tr '\0' '\n' | grep ^USER    | cut -d = -f 2)
        done | sort | uniq | while read DISPLAY USER ; do
          [ -z "$(grep "^$USER:" /etc/shadow | cut -d : -f 2)" ] && continue # if password empty
          export DISPLAY
          ${config.security.wrapperDir}/sudo --background -u $USER bash -c \
            'pkill -u $USER -USR1 dunst
             i3lock -n -c 000000 || true
             pkill -u $USER -USR2 dunst'
        done
        '';
    };

    security.sudo = {
      enable = true;
      extraConfig = ''
        %users      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl start lock-x11-displays
        '';
    };

  };
}
