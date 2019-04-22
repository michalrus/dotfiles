{ config, lib, pkgs, ... }:

{

  networking.networkmanager.enable = true;

  environment.gnome3.excludePackages = [ pkgs.gnome3.epiphany ];

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

  # use Xorg instead of Wayland
  environment.etc."gdm/custom.conf".text = ''
    [daemon]
    WaylandEnable=false
  '';

  # https://github.com/NixOS/nixpkgs/issues/24172#issuecomment-293714795
  systemd.targets."multi-user".conflicts = [ "getty@tty1.service" ];

  # TODO: Also, try this: https://github.com/NixOS/nixpkgs/issues/24172#issuecomment-330334844
  # nixpkgs.overlays = [ (self: super: {
  #   gnome3 = super.gnome3 // {
  #     gdm = super.gnome3.gdm.overrideAttrs (oldAttrs: {
  #       configureFlags = oldAttrs.configureFlags ++ [ "--with-initial-vt=7" ];
  #     });
  #   };
  # }) ];

}
