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
  };

  environment.systemPackages = with pkgs; [
    arandr
    ardour4
    audacity
    awf
    calibre
    chromium
    cool-retro-term
    frescobaldi
    gimp
    gnome3.aisleriot
    gnome3.file-roller
    gtk  # Why? Icon cache! See #20874.
    libnotify
    libreoffice
    lilypond
    mpv
    networkmanagerapplet
    octave
    openjdk8
    simple-scan
    skype
    system-config-printer # For GNOME Printers applet.
    transcribe
    transmission_gtk
    unclutter
    wine
    winetricks
    wmctrl
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
    youtube-dl
  ];

  services = {
    #udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.

    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandlePowerKey=hibernate
    '';

    xserver = {
      synaptics.enable = false; # GNOME uses libinput.
      displayManager.gdm.enable = true;
      desktopManager.xterm.enable = false;
      desktopManager.gnome3.enable = true;

      displayManager.xserverArgs = [ "-ardelay" "150" "-arinterval" "8" ];
    };
  };

  fonts.fonts = with pkgs; [
    corefonts
    vistafonts
  ];

  # For profile pictures, see #20872.

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" ];
    };

    extraUsers.mikolaj = {
      hashedPassword = "$6$Mhe4HFJEEu5WL$vr09OpHztpUwnZk/PvNqvZI1dQI.zlfmcE/EiYvJvAE0HcDZJ/YvYc6pzqGhitRjrVklyCCIemSUl0EzZmGhL.";
      isNormalUser = true;
      description = "Mikolaj Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" ];
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
