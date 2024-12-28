{ on-vt-switch-src }:

{ config, pkgs, lib, ... }:

let

  on-vt-switch = pkgs.callPackage on-vt-switch-src {};

in

{
  systemd.services."on-vt-switch-lock" = {
    wantedBy = [ "multi-user.target" ];
    after = [ "getty.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 1;
      ExecStart = let
        handleEvent = pkgs.writeScript "handle-event" ''
          #! ${pkgs.stdenv.shell}
          exec ${config.systemd.package}/bin/loginctl lock-sessions
        '';
      in "${on-vt-switch}/bin/on-vt-switch ${handleEvent}";
    };
  };

  ### TODO: locking textual VTs… hmm — it seems the only option is to… kill their `login` process…
  ###   • each swaylock/i3lock has XDG_SESSION_ID set in /proc/XXX/environ
  ###   • so list all sessions `loginctl list-sessions`…
  ###   • … and `systemctl stop session-XXX.scope` for sessions without i3lock / swaylock

  # Lock screen on anything HID, cf. https://youtu.be/5Nk6iDryW0Y?t=1323 .
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="hid", RUN+="${config.systemd.package}/bin/loginctl lock-sessions"
  '';
}
