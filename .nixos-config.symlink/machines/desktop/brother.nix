{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    #./modules/android.nix
    ./modules/chwalecice.nix
    ./modules/gnome.nix
  ];

  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "en_US.UTF-8";

  nixpkgs.config = {
    allowUnfree = true; # M$ fonts, Skype™ and similar nonsense.
  };

  environment.systemPackages = with pkgs; [
    arandr
    audacity
    awf
    calibre
    (chromium.override { enablePepperFlash = true; })
    cool-retro-term
    frescobaldi
    gimp
    gnome3.aisleriot
    gnucash26
    gtk2  # Why? Icon cache! See #20874.
    inkscape
    ioquake3
    libreoffice
    lilypond
    mpv
    mumble
    octave
    openjdk8
    steam
    simple-scan
    skype
    teamspeak_client
    transcribe
    transmission_gtk
    unrar
    xarchiver
    xmind
    xsane
    youtube-dl
  ];

  services = {
    #udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.

    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandlePowerKey=hibernate
    '';

    xserver = {
      xkbOptions = "ctrl:nocaps,compose:caps";
      displayManager.xserverArgs = [ "-ardelay" "150" "-arinterval" "8" ];
    };
  };

  fonts.fonts = with pkgs; [
    corefonts
    eb-garamond
    helvetica-neue-lt-std
    liberation_ttf
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
}
