{ config, pkgs, lib, ... }:

let

  lockVTs = pkgs.writeScriptBin "lock-vts" ''
    #! ${pkgs.stdenv.shell}

    set -o errexit
    export PATH=${lib.makeBinPath (with pkgs; [ procps gawk ])}:$PATH

    ### sway

    active_lock_envs=$(pgrep '^swaylock$' | awk '{ print "/proc/" $1 "/environ" }')
    active_sockets=$(find /run/user -path '/run/user/*/sway-ipc.*.sock')

    for sock in $active_sockets ; do
      if [ -z "$active_lock_envs" ] || ! grep -F $sock $active_lock_envs >/dev/null ; then
        echo >&2 "Locking: $sock…"
        SWAYSOCK=$sock ${pkgs.sway}/bin/swaymsg exec ${lock-sway}
      else
        echo >&2 "Already locked: $sock"
      fi
    done

    ### i3

    active_lock_envs=$(pgrep '^i3lock$' | awk '{ print "/proc/" $1 "/environ" }')
    active_sockets=$(find /run/user -path '/run/user/*/i3/ipc-socket.*')

    for sock in $active_sockets ; do
      if [ -z "$active_lock_envs" ] || ! grep -F $sock $active_lock_envs >/dev/null ; then
        echo >&2 "Locking: $sock…"
        I3SOCK=$sock ${pkgs.i3}/bin/i3-msg -q exec ${lock-i3}
      else
        echo >&2 "Already locked: $sock"
      fi
    done

    ### text-mode VT

    ###########################
    #
    # • locking textual VTs… hmm
    #   • it seems the only option is to… kill their `login` process…
    #
    ###########################
  '';

  # <https://github.com/swaywm/swaylock/issues/49#issuecomment-462143402>
  lock-sway = pkgs.writeScript "lock-sway" ''
    #! ${pkgs.stdenv.shell}
    export PATH=${lib.makeBinPath (with pkgs; [ sway ])}:$PATH
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
    description = "Lock all VTs";
    serviceConfig = {
      Type = "oneshot";
      ExecStart = "${lockVTs}/bin/lock-vts";
    };
    wantedBy = [ "sleep.target" ];
    before = [ "sleep.target" ];
  };

  ### TODO: uncomment this on bare-metal:

  # Lock screen on anything HID, cf. https://youtu.be/5Nk6iDryW0Y?t=1323 .
  # services.udev.extraRules = ''
  #   ACTION=="add", SUBSYSTEM=="hid", RUN+="${config.systemd.package}/bin/systemctl restart lock-vts.service"
  # '';

  security.sudo = {
    enable = true;
    extraConfig = ''
      %users ALL = (root) NOPASSWD: ${config.systemd.package}/bin/systemctl start lock-vts
      '';
  };

}
