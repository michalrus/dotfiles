{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    #./modules/android.nix
    ./modules/chwalecice.nix
    ./modules/gnome.nix
  ];
  hardware.pulseaudio = {
    enable = true;
    # NixOS allows either a lightweight build (default) or full build of PulseAudio to be installed.
    # Only the full build has Bluetooth support, so it must be selected here.
    package = pkgs.pulseaudioFull;
  };
  hardware.bluetooth.enable = true;
  hardware.bluetooth.package = pkgs.bluezFull;
  hardware.bluetooth.config.Audio.Disable = "socket";
  hardware.opengl.driSupport32Bit = true;
  hardware.opengl.extraPackages32 = with pkgs.pkgsi686Linux; [ libva ];
  hardware.pulseaudio.support32Bit = true;
  boot.extraModprobeConfig = ''options snd_hda_intel model=generic'';
  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "en_US.UTF-8";

  environment.systemPackages = with pkgs; [
    (nixos-unstable.wine.override { pulseaudioSupport = true; wineBuild = "wineWow"; })
    anki
    blueman
    chromium
    dosbox
    frescobaldi
    gnome3.pomodoro
    ioquake3
    jre8
    lilypond
    moltengamepad
    mumble
    openjdk8
    playonlinux
    transmission_gtk
    qjoypad
    unfree.google-chrome
    #unfree.michalrus.transcribe
    unfree.michalrus.steam
    unfree.skype
    unfree.teams
    unfree.teamspeak_client
    unfree.unrar
    unfree.xmind
    wxhexeditor
    xboxdrv

    arandr
    wmctrl xtitle
    xrandr-invert-colors
    unclutter xbanish
    xautolock

    xcape xdo xdotool
    xclip xsel
    xpad
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
