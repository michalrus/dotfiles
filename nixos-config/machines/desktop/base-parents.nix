{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./modules/android.nix
    ./modules/malicious-hosts.nix
    ./modules/chwalecice.nix
  ];

  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "pl_PL.UTF-8";

  environment.systemPackages = with pkgs; [
    (nixos-unstable.wine.override { pulseaudioSupport = true; })
    nixos-unstable.gnome3.evolution
    transmission_gtk
    unfree.google-chrome
    unfree.skype
    unfree.unrar

    arandr
    wmctrl xtitle
    xrandr-invert-colors
    unclutter xbanish
    xautolock

    xcape xdo xdotool
    xclip xsel
    xpad
  ];

  services = {
    udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.
  };

  fonts.fonts = with pkgs; [
    unfree.corefonts
    unfree.vistafonts
  ];
}
