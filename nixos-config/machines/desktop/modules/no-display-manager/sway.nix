{ config, pkgs, lib, ... }:

let

  ulib = import ./ulib.nix { inherit config pkgs; };

  #
  # There’s no `wofi` in nixos-19.09 yet, let’s use `nixos-unstable`
  # for that (plus some patch).
  #
  wofiUnstable = (import (pkgs.fetchFromGitHub  {
    owner = "NixOS"; repo = "nixpkgs";
    rev = "ae6bdcc53584aaf20211ce1814bea97ece08a248";
    sha256 = "0hjhznns1cxgl3hww2d5si6vhy36pnm53hms9h338v6r633dcy77";
  }) {}).wofi.overrideAttrs (oldAttrs: {
    patches = [
      (pkgs.fetchpatch {
        url = "https://paste.sr.ht/blob/1cbddafac3806afb203940c029e78ce8390d8f49";
        sha256 = "1n4jpmh66p7asjhj0z2s94ny91lmaq4hhh2356nj406vlqr15vbb";
      })
    ];
  });

  start-sway = pkgs.writeScript "start-sway" ''
    #! ${pkgs.stdenv.shell}

    ${ulib.exportProfileWithPkgs "sway" (with pkgs; [

      # These packages will be visible from within `sway` session only.
      dbus
      sway swayidle swaylock xwayland wofiUnstable
      termite

      firefox-wayland

      # For Xwayland’s DPI setting:
      xorg.xeyes xorg.xdpyinfo xorg.xrandr xorg.xrdb

    ])}

    export QT_QPA_PLATFORM=wayland-egl
    export QT_WAYLAND_FORCE_DPI=physical
    export QT_WAYLAND_DISABLE_WINDOWDECORATION=1
    export ECORE_EVAS_ENGINE=wayland_egl
    export ELM_ENGINE=wayland_egl
    export SDL_VIDEODRIVER=wayland
    export _JAVA_AWT_WM_NONREPARENTING=1
    export MOZ_ENABLE_WAYLAND=1
    export SAL_USE_VCLPLUGIN=gtk3

    exec dbus-launch --exit-with-session systemd-cat -t sway sway
  '';

in

{

  security.pam.services.swaylock = {};
  hardware.opengl.enable   = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  # Set up aliases *only* in a pure TTY virtual terminal, to run right
  # after agetty login. After exiting sway, you will be logged out
  # cleanly.
  environment.extraInit = ulib.ifTTY ''
    alias sway='clear && exec ${start-sway}'

    # Optionally:
    alias startw=sway
  '';

}
