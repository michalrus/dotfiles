{ config, lib, pkgs, ... }:

{

  networking.networkmanager.enable = true;

  nixpkgs.overlays = [ (self: super: {
    # By default, gvfs in Xfce has no Samba support. Turn it back on.
    xfce = super.xfce // { gvfs = self.gvfs; };
    # Feh is always added to system PATH, see #17450.
    feh = super.feh.overrideDerivation(oldAttrs: { postInstall = "rm $out/share/applications/feh.desktop"; });
  }) ];

  security.pam.services.slimlock = {};

  environment.systemPackages = with pkgs; [
    (runCommand "wrap-slimlock" {} "mkdir -p $out/bin && ln -s ${pkgs.slim}/bin/slimlock $out/bin/slock")
    galculator
    gnome2.gnome_icon_theme
    gnome3.adwaita-icon-theme
    gnome3.cheese
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.file-roller # for thunar-archive-plugin
    gnome3.gnome_themes_standard
    gtk2 # Why? Icon cache! See #20874.
    networkmanagerapplet
    system-config-printer # For GNOME Printers applet.
  ];

  services = {
    gnome3.gnome-keyring.enable = true;

    xserver = {
      displayManager.lightdm.enable = lib.mkDefault true;
      desktopManager.xterm.enable = false;
      desktopManager.xfce = {
        enable = true;
        thunarPlugins = with pkgs.xfce; [ thunar-archive-plugin ];
      };

      # For Evolution password management
      displayManager.sessionCommands = ''
        gnome-keyring-daemon || true
      '';
    };
  };

}
