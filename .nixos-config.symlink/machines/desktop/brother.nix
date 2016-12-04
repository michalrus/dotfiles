{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    #./modules/android.nix
  ];

  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "en_US.UTF-8";

  hardware = {
    sane.extraConfig.pixma = "bjnp://10.0.1.5";
  };

  nixpkgs.config = {
    allowUnfree = true; # M$ fonts, Skype™ and similar nonsense.

    packageOverrides = super: let self = super.pkgs; in {
      # Feh is always added to system PATH, see #17450.
      feh = super.feh.overrideDerivation(oldAttrs: { postInstall = "rm $out/share/applications/feh.desktop"; });
    };
  };

  environment.systemPackages = with pkgs; [
    awf
    chromium
    cool-retro-term
    evince
    gnome3.aisleriot
    gnome3.file-roller
    gtk  # Why? Icon cache! See #20874.
    libnotify
    libreoffice
    mpv
    networkmanagerapplet
    simple-scan
    skype
    transmission_gtk
    unclutter
    wine
    xarchiver
    xclip
    xdotool
    xorg.xbacklight
    xorg.xev
    xorg.xdpyinfo
    xorg.xmodmap
    xrandr-invert-colors
    xsane
    xsel
  ];

  services = {
    #udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.

    xserver = {
      synaptics.enable = false;
#      synaptics = {
#        maxSpeed = "5.0";
#        accelFactor = "0.025";
#      };

      displayManager.gdm.enable = lib.mkDefault true;
      desktopManager.xterm.enable = false;
      desktopManager.gnome3.enable = true;
    };
  };

  fonts.fonts = with pkgs; [
    corefonts
    vistafonts
  ];

  # For profile pictures, see #20872.

  users.extraUsers.mikolaj = {
    hashedPassword = "$6$W/KppVZSY$.vf1jfCd6H0tOJwRwmUwJeMSkmg/MyDUlNpx3IRHWjmLpyXyg5quW0VRBX4QwGp00MIT6Nw2nODs.JhleHblz1";
    isNormalUser = true;
    description = "Mikolaj Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
