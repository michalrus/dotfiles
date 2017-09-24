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

  nixpkgs.config = {
    allowUnfree = true; # M$ fonts, Skype™ and similar nonsense.
  };

  # Use GTK 2 in LibreOffice. 3 has some menu rendering problems.
  environment.variables."SAL_USE_VCLPLUGIN" = "gtk";

  environment.systemPackages = with pkgs; [
    google-chrome
    cool-retro-term
    evince
    gimp
    gnome3.aisleriot
    gnome3.evolution
    libreoffice
    mpv
    python34Packages.livestreamer
    rtmpdump
    simple-scan
    skype
    transmission_gtk
    xarchiver
    michalrus.xpad
    xsane
  ];

  services = {
    udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.
  };

  fonts.fonts = with pkgs; [
    corefonts
    vistafonts
  ];
}
