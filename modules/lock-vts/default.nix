{ on-vt-switch-src }:

{ config, pkgs, lib, ... }:

let

  on-vt-switch = pkgs.callPackage on-vt-switch-src {};

  lockVTs = pkgs.writeScriptBin "lock-vts" ''
    #! ${pkgs.stdenv.shell}

    set -o errexit
    export PATH=${lib.makeBinPath (with pkgs; [ procps gawk ])}:$PATH

    ### sway

    active_lock_envs=$(pgrep '^swaylock$' | awk '{ print "/proc/" $1 "/environ" }')
    active_sockets=$(find /run/user -path '/run/user/*/sway-ipc.*.sock' 2>/dev/null || true)

    for sock in $active_sockets ; do
      if [ -z "$active_lock_envs" ] || ! grep -F $sock $active_lock_envs >/dev/null ; then
        SWAYSOCK=$sock ${pkgs.sway}/bin/swaymsg -q exec ${lock-sway}
      fi
    done

    ### i3

    active_lock_envs=$(pgrep '^i3lock$' | awk '{ print "/proc/" $1 "/environ" }')
    active_sockets=$(find /run/user -path '/run/user/*/i3/ipc-socket.*' 2>/dev/null || true)

    for sock in $active_sockets ; do
      if [ -z "$active_lock_envs" ] || ! grep -F $sock $active_lock_envs >/dev/null ; then
        I3SOCK=$sock ${pkgs.i3}/bin/i3-msg -q exec ${lock-i3}
      fi
    done

    ### hyprland

    active_lock_envs=$(pgrep '^hyprlock$' | awk '{ print "/proc/" $1 "/environ" }')
    active_sockets=$(find /run/user -path '/run/user/*/hypr/*/.socket.sock' 2>/dev/null || true)

    for sock in $active_sockets ; do
      inst_sig=$(basename "$(dirname "$sock")")
      if [ -z "$active_lock_envs" ] || ! grep -F "$inst_sig" $active_lock_envs >/dev/null ; then
        echo "dispatch exec hyprlock" | ${pkgs.socat}/bin/socat - UNIX-CONNECT:"$sock"
      fi
    done

    ### text-mode VT

    # ?
  '';

  exitOnEmptyPassword = ''
    LC_ALL=c passwd 0</dev/null 2>&1 | grep '^New password:' >/dev/null && exit || true
  '';

  # <https://github.com/swaywm/swaylock/issues/49#issuecomment-462143402>
  lock-sway = pkgs.writeScript "lock-sway" ''
    #! ${pkgs.stdenv.shell}
    export PATH=${lib.makeBinPath (with pkgs; [ sway ])}:$PATH
    ${exitOnEmptyPassword}
    swayidle \
      timeout 3 'swaymsg "output * dpms off"' \
      resume 'swaymsg "output * dpms on"' &
    swayidle_pid=$!
    revert() {
      kill $swayidle_pid
    }
    trap revert HUP INT TERM EXIT
    swaylock -F -l -c000000
  '';

  lock-i3 = pkgs.writeScript "lock-i3" ''
    #! ${pkgs.stdenv.shell}
    export PATH=${lib.makeBinPath (with pkgs; [ procps xorg.xset i3 i3lock ])}:$PATH
    ${exitOnEmptyPassword}
    revert() {
      pkill -u $USER -USR2 dunst
      xset dpms 0 0 0
    }
    trap revert HUP INT TERM EXIT
    pkill -u $USER -USR1 dunst
    xset +dpms dpms 3 3 3
    i3lock -n -c 000000
  '';

in

{

  systemd.services."lock-vts" = {
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lockVTs}/bin/lock-vts";
    };
    wantedBy = [ "sleep.target" ];
    before = [ "sleep.target" ];
  };

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
          exec ${config.systemd.package}/bin/systemctl --wait start lock-vts
        '';
      in "${on-vt-switch}/bin/on-vt-switch ${handleEvent}";
    };
  };

  # Add Super+L to global key bindings
  services.actkbd = {
    enable = true;
    bindings = [
      # keycode 125 = Super_L
      # keycode  38 = l
      { keys = [ 125 38 ]; events = [ "key" ]; command = "${config.systemd.package}/bin/systemctl start lock-vts"; }
    ];
  };

  ### TODO: rethink

  ### TODO: Spacemacs

  ### TODO: configure sway like i3

  ### TODO: notifications: sway: mako + check on lockscreens

  ### TODO: locking textual VTs… hmm — it seems the only option is to… kill their `login` process…
  ###   • each swaylock/i3lock has XDG_SESSION_ID set in /proc/XXX/environ
  ###   • so list all sessions `loginctl list-sessions`…
  ###   • … and `systemctl stop session-XXX.scope` for sessions without i3lock / swaylock

  ### TODO: kitty

  # Lock screen on anything HID, cf. https://youtu.be/5Nk6iDryW0Y?t=1323 .
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="hid", RUN+="${config.systemd.package}/bin/systemctl restart lock-vts.service"
  '';

}
