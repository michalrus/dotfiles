{ config, lib, pkgs, ... }:

{

  networking.networkmanager.enable = true;

  environment.gnome3.excludePackages = [ pkgs.gnome3.epiphany ];

  environment.systemPackages = with pkgs; [
    gnome3.cheese
    gnome3.file-roller
    gnome2.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnome3.bijiben
    gtk2 # Why? Icon cache! See #20874.
    networkmanagerapplet
    system-config-printer # For GNOME Printers applet.
  ];

  services = {
    xserver = {
      synaptics.enable = false; # GNOME uses libinput.
      displayManager.gdm.enable = true;
      desktopManager.xterm.enable = false;
      desktopManager.gnome3.enable = true;
    };
  };

  # https://github.com/NixOS/nixpkgs/issues/24172#issuecomment-293714795
  systemd.targets."multi-user".conflicts = [ "getty@tty1.service" ];

}
