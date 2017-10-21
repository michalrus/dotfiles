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
    wine.build = "wineWow"; # for some 64-bit games
  };

  environment.systemPackages = with pkgs; [
    (chromium.override { enablePepperFlash = true; })
    anki
    frescobaldi
    gnome3.pomodoro
    gnucash26
    ioquake3
    lilypond
    michalrus.transcribe
    mumble
    openjdk8
    skype
    steam
    teamspeak_client
    unrar
    xmind
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

    users.guest.dotfiles = let d = ../../../dotfiles; in [ "${d}/base" ];

    extraUsers.mikolaj = {
      hashedPassword = "$6$Mhe4HFJEEu5WL$vr09OpHztpUwnZk/PvNqvZI1dQI.zlfmcE/EiYvJvAE0HcDZJ/YvYc6pzqGhitRjrVklyCCIemSUl0EzZmGhL.";
      isNormalUser = true;
      description = "Mikolaj Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" ];
      dotfiles = let d = ../../../dotfiles; in [ "${d}/base" "${d}/gnome" "${d}/git-annex" "${d}/mikolajrus" ];
    };
  };
}
