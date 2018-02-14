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
    wine.build = "wineWow"; # for some 64-bit games
  };

  environment.systemPackages = with pkgs; [
    (chromium.override { enablePepperFlash = true; })
    anki
    dosbox
    frescobaldi
    gnome3.pomodoro
    ioquake3
    lilypond
    michalrus.gnucash26
    mumble
    openjdk8
    unfree.michalrus.transcribe
    unfree.nixos-unstable.steam
    unfree.skype
    unfree.teamspeak_client
    unfree.unrar
    unfree.xmind
  ];

  services = {
    #udev.packages = [ pkgs.libmtp.bin ]; # For Android in GVFS, see #6304.

    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandlePowerKey=hibernate
    '';

    xserver = {
      xkbOptions = "ctrl:nocaps,compose:caps";
    };
  };

  fonts.fonts = with pkgs; [
    eb-garamond
    liberation_ttf
    unfree.corefonts
    unfree.helvetica-neue-lt-std
    unfree.vistafonts
  ];

  # For profile pictures, see #20872.

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" ];
    };

    users.guest.dotfiles.profiles = [ "base" ];

    extraUsers.mikolaj = {
      hashedPassword = "$6$Mhe4HFJEEu5WL$vr09OpHztpUwnZk/PvNqvZI1dQI.zlfmcE/EiYvJvAE0HcDZJ/YvYc6pzqGhitRjrVklyCCIemSUl0EzZmGhL.";
      isNormalUser = true;
      description = "Mikolaj Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" ];
      dotfiles.profiles = [ "base" "gnome" "git-annex" "mikolajrus" ];
    };
  };
}
