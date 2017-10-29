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

  environment.systemPackages = with pkgs; [
    nixos-unstable.evolution
    google-chrome
    skype
    unrar
  ];

  services = {
    udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.
  };

  fonts.fonts = with pkgs; [
    corefonts
    vistafonts
  ];
}
