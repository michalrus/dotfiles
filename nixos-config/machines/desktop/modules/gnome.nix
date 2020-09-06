{ config, lib, pkgs, ... }:

{

  networking.networkmanager.enable = true;

  environment.gnome3.excludePackages = [ pkgs.gnome3.epiphany ];

  services.dbus.packages = with pkgs; [ gnome2.GConf ];
  services.spice-vdagentd.enable = true;

  environment.systemPackages = with pkgs; [
    gnome3.file-roller
    gnome2.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gthumb
    gtk2 # Why? Icon cache! See #20874.
    networkmanagerapplet
    nixos-unstable.system-config-printer # For GNOME Printers applet.
  ];

  services = {
    xserver = {
      synaptics.enable = false; # GNOME uses libinput.
      displayManager.gdm.enable = true;
      desktopManager.xterm.enable = false;
      desktopManager.gnome3.enable = true;
    };
  };

}
