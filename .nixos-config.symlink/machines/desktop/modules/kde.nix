{ config, lib, pkgs, ... }:

{

  networking.networkmanager.enable = true;

  environment.systemPackages = with pkgs; [
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.bijiben
    networkmanagerapplet
    system-config-printer # For GNOME Printers applet.
  ];

  services = {
    xserver = {
      displayManager.sddm.enable = true;
      desktopManager.xterm.enable = false;
      desktopManager.plasma5.enable = true;
    };
  };

}
