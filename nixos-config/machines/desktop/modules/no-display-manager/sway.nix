{ config, pkgs, lib, ... }:

let

  ulib = import ./ulib.nix { inherit config pkgs; };

  start-sway = pkgs.writeScript "start-sway" ''
    #! ${pkgs.stdenv.shell}

    . ${config.environment.dynamic-profiles.sway.loadFile}

    exec dbus-launch --exit-with-session systemd-cat -t sway ${pkgs.writeScript "sway" ''
      #! ${pkgs.stdenv.shell} -l

      # Starting it via login shell, to allow user to set their own environment variables in ~/.profile:
      exec $SHELL -l -c "exec sway"
    ''}
  '';

in

{

  environment.dynamic-profiles.sway = {
    packages = with pkgs; [

      # These packages will be visible from within `sway` session only.
      sway swayidle swaylock xwayland nixos-unstable.wofi
      termite firefox-wayland

      # For Xwayland’s DPI setting:
      xorg.xeyes xorg.xdpyinfo xorg.xrandr xorg.xrdb

    ];

    extraSetup = ''
      export QT_QPA_PLATFORM=wayland-egl
      export QT_WAYLAND_FORCE_DPI=physical
      export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
      export ECORE_EVAS_ENGINE=wayland_egl
      export ELM_ENGINE=wayland_egl
      export SDL_VIDEODRIVER=wayland
      export _JAVA_AWT_WM_NONREPARENTING=1
      export MOZ_ENABLE_WAYLAND=1
      export SAL_USE_VCLPLUGIN=gtk3

      export DESKTOP_SESSION=sway
    '';

    extraPostBuild = ''
      ln -s ${start-sway} $out/start-sway
    '';
  };

  security.pam.services.swaylock = {};
  hardware.opengl.enable   = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  # Set up aliases *only* in a pure TTY virtual terminal, to run right
  # after agetty login. After exiting sway, you will be logged out
  # cleanly.
  environment.extraInit = ulib.ifTTY ''
    alias sway='clear && exec ${builtins.dirOf config.environment.dynamic-profiles.sway.loadFile}/start-sway'

    # Optionally:
    alias startw=sway
  '';

  ###
  ### FIXME: instead, link `start-sway` in /etc/profiles/dynamic/sway/start-sway
  ###
  environment.systemPackages = [ (pkgs.writeTextDir "prevent-gc" (toString [ start-sway ])) ];

}
