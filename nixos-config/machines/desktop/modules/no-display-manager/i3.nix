{ config, pkgs, lib, ... }:

let

  ulib = import ./ulib.nix { inherit config pkgs; };

  #
  # Optionally, merge configs, because `i3` doesn’t have an
  # `include` directive… <https://github.com/i3/i3/issues/1197>
  #
  # To use that, create a few ~/.config/i3/*.conf files.
  #
  # Note, that if you use `*.conf` instead of a single `config`,
  # this will effectively break `i3-msg reload`.
  #
  i3MergedConfigs = pkgs.writeScript "i3-merged-configs" ''
    #! ${pkgs.stdenv.shell}

    configOpt=""
    if ls ~/.config/i3/*.conf 1>/dev/null 2>&1 ; then
      dir=$XDG_RUNTIME_DIR/i3
      mkdir -p $dir
      cnf=$(mktemp -p $dir config.XXXXXXX)
      find ~/.config/i3 -name '*.conf' -print0 | sort -z | xargs -r0 cat > $cnf
      configOpt=" -c $cnf "
    fi

    exec i3 $configOpt
  '';

  start-i3 = pkgs.writeScript "start-i3" ''
    #! ${pkgs.stdenv.shell}

    ${ulib.exportProfileWithPkgs "i3" (with pkgs; [

      # These packages will be visible from within `i3` session only.
      dbus
      i3 i3lock i3status dmenu
      termite

      firefox

      xorg.xorgserver xorg.xauth xkeyboard_config
      xterm xorg.xeyes xorg.xclock
      xorg.xdpyinfo xorg.xrandr xorg.xrdb xorg.xset

    ])}

    exec dbus-launch --exit-with-session systemd-cat -t i3 ${ulib.do-startx i3MergedConfigs}
  '';

in

{

  security.pam.services.i3lock = {};
  hardware.opengl.enable   = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  # Set up aliases *only* in a pure TTY virtual terminal, to run right
  # after agetty login. After exiting i3, you will be logged out
  # cleanly.
  environment.extraInit = ulib.ifTTY ''
    alias i3='clear && exec ${start-i3}'

    # Optionally:
    alias startx=i3
  '';

}
