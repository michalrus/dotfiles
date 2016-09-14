{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./modules/media.nix
    ./modules/android.nix
    ./my-wifi-passwords.nix
  ];

  boot.tmpOnTmpfs = true;

  powerManagement = {
    powerDownCommands = ''
      ${pkgs.procps}/bin/pgrep ssh | while IFS= read -r pid ; do
        [ "$(readlink "/proc/$pid/exe")" = "${pkgs.openssh}/bin/ssh" ] && kill "$pid"
      done
    '';
    powerUpCommands = ''
      ${pkgs.eject}/bin/eject -i on /dev/sr0 # Why isn’t this working‽ How to block the CD eject button?
    '';
  };

  networking = {
    connman = {
      enable = true;
      # https://wiki.archlinux.org/index.php/Connman#Avoid_changing_the_hostname
      extraConfig = ''
        [General]
        AllowHostnameUpdates=false
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    arandr
    awf
    cdrkit
    compton
    conkeror
    conky
    cool-retro-term
    dmenu
    dunst
    emacs
    evince
    firefox
    gettext
    gettext-emacs
    ghostscript
    gnome.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnucash26
    gparted
    # (haskellPackages.ghcWithHoogle (haskellPackages: with haskellPackages; [
    (haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [
      cabal-install happy hindent hlint parallel stylish-haskell turtle
      # ghc-mod hasktags
    ]))
    isync
    libnotify
    mu
    oathToolkit
    openjdk8
    pass
    pavucontrol
    pinentry
    scala
    st
    (texlive.combine {
      inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages;
      gregorio = pkgs.gregorio.forTexlive;
    })
    transmission_gtk
    utox
    visualvm
    (wine.override { pulseaudioSupport = true; })
    winetricks
    wmctrl
    xarchiver
    xautolock
    xcape
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

  environment.variables."GTK2_RC_FILES" =
    "${pkgs.gnome3.gnome_themes_standard}/share/themes/Adwaita/gtk-2.0/gtkrc";

  programs = {
    ssh.startAgent = false;
    wireshark.enable = true;
  };

  virtualisation.virtualbox.host.enable = true;

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandlePowerKey=suspend
    '';

    lockX11Displays.enable = true;

    xserver = {
      synaptics = {
        maxSpeed = "4.0";
        accelFactor = "0.02";
        additionalOptions = ''
          Option "VertScrollDelta" "-114"
          Option "HorizScrollDelta" "-114"
        '';
      };

      displayManager.lightdm.enable = true;
      desktopManager.xterm.enable = false;
      windowManager.i3.enable = true;

      displayManager.xserverArgs = [ "-ardelay" "150" "-arinterval" "8" ];
    };
  };

  fonts.fonts = with pkgs; [
    anonymousPro
    hack-font
    inconsolata
  ];

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "nonet" "scanner" "networkmanager" "vboxusers" ];
    };

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      linger = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" ];
    };
  };

  system.autoUpgrade.enable = false;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
