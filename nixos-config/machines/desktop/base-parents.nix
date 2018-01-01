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
    nixos-unstable.gnome3.evolution
    unfree.google-chrome
    unfree.skype
    unfree.unrar
  ];

  services = {
    udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.
  };

  fonts.fonts = with pkgs; [
    unfree.corefonts
    unfree.vistafonts
  ];
}
