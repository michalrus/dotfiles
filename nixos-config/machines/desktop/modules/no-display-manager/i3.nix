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

    # Starting it via login shell, to allow user to set their own environment variables in ~/.profile:
    exec $SHELL -l -c "exec i3 $configOpt"
  '';

  start-i3 = pkgs.writeScript "start-i3" ''
    #! ${pkgs.stdenv.shell}

    . ${config.environment.dynamic-profiles.i3.loadFile}

    exec dbus-launch --exit-with-session systemd-cat -t i3 ${ulib.do-startx i3MergedConfigs}
  '';

in

{

  environment.dynamic-profiles.i3 = {
    packages = with pkgs; [

      # These packages will be visible from within `i3` session only.
      i3 i3lock i3status
      rofi michalrus.dmenu-rofi dunst
      compton
      autocutsel
      peek

      termite firefox

      arandr
      wmctrl xtitle
      xrandr-invert-colors
      unclutter xbanish
      xautolock

      xcape xdo xdotool
      xclip xsel
      xpad

    ] ++ (with pkgs.xorg; [

      xkeyboard_config
      xorgserver xauth xkeyboard_config
      xev xdpyinfo xrandr xrdb xset xinput xprop
      xterm xeyes xclock
      xbacklight xhost xmodmap

    ]);

    extraSetup = ''
      export TERMINAL=termite
      export _JAVA_AWT_WM_NONREPARENTING=1
      export XMODIFIERS="@im=none"  # For ~/.XCompose to…
      export GTK_IM_MODULE=xim      #        … work in Gtk apps
      export MOZ_USE_XINPUT2=1      # For true Firefox smooth scrolling with touchpad.

      export DESKTOP_SESSION=i3
    '';

    extraPostBuild = ''
      ln -s ${start-i3} $out/start-i3
    '';
  };

  security.pam.services.i3lock = {};
  hardware.opengl.enable   = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  # Set up aliases *only* in a pure TTY virtual terminal, to run right
  # after agetty login. After exiting i3, you will be logged out
  # cleanly.
  environment.extraInit = ulib.ifTTY ''
    alias i3='clear && exec ${builtins.dirOf config.environment.dynamic-profiles.i3.loadFile}/start-i3'

    # Optionally:
    alias startx=i3
  '';

  ###
  ### FIXME: instead, link `start-i3` in /etc/profiles/dynamic/i3/start-i3
  ###
  environment.systemPackages = [ (pkgs.writeTextDir "prevent-gc" (toString [ start-i3 ])) ];

}
