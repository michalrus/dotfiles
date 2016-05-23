{ config, pkgs, ... }:

{
  imports = [
    ./modules
    ./pkgs
    ./local
    ./common.nix
    ./hardware-configuration.nix
  ];

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
    initrd.luks.devices = [ { name = "crypt"; device = "/dev/sda2"; } ];
    tmpOnTmpfs = true;
  };

  powerManagement = {
    cpuFreqGovernor = "performance";
    powerDownCommands = ''
      ${pkgs.procps}/bin/pgrep ssh | while IFS= read -r pid ; do
        [ "$(readlink "/proc/$pid/exe")" = "${pkgs.openssh}/bin/ssh" ] && kill "$pid"
      done
      '';
    powerUpCommands = ''
      ${pkgs.eject}/bin/eject -i on /dev/sr0 # Why isn’t this working‽ How to block the CD eject button?
      '';
  };

  networking = rec {
    hostName = "nixos";
    extraHosts = "127.0.0.1 ${hostName}";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ ];
      nonetGroup.enable = true;
    };
    wireless.enable = true;
  };

  time.timeZone = "Europe/Warsaw";

  hardware = {
    sane.enable = true;
    opengl.driSupport32Bit = true; # for Wine
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

  nixpkgs.config = {
    allowBroken = true;
    pulseaudio = true;
  };

  environment.systemPackages = with pkgs; [
    arandr
    awf-gtk
    calibre
    compton
    conkeror-unwrapped
    conky
    cool-retro-term
    dmenu
    dunst
    emacs
    evince
    gimp
    gnome.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnucash26
    (haskellPackages.ghcWithHoogle (haskellPackages: with haskellPackages; [
      cabal-install ghc-mod happy hasktags hindent hlint parallel stylish-haskell turtle
    ]))
    isync
    jmeter
    jmtpfs
    libmtp
    libnotify
    mpv
    mu
    oathToolkit
    openjdk8
    pass
    pavucontrol
    pinentry
    #pkgs.firefoxWrapper
    ((wrapFirefox.override { libpulseaudio = libpulseaudio.out; }) firefox-unwrapped { }) # temporary solution for https://github.com/NixOS/nixpkgs/issues/15126
    python34Packages.livestreamer
    rtmpdump
    scala
    st
    (texlive.combine {
      inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons;
    })
    transmission_gtk
    utox
    visualvm
    (wine.override { pulseaudioSupport = true; })
    winetricks
    wmctrl
    xautolock
    xcape
    xclip
    xdotool
    xorg.xbacklight
    xorg.xev
    xorg.xdpyinfo
    xorg.xmodmap
    xrandr-invert-colors
    xsel
  ];

  environment.shellInit = ''
    export GTK2_RC_FILES=${pkgs.gnome3.gnome_themes_standard}/share/themes/Adwaita/gtk-2.0/gtkrc
    '';

  programs = {
    ssh.startAgent = false;
    wireshark.enable = true;
  };

  services = {
    logind.extraConfig = ''
        HandleLidSwitch=suspend
        HandlePowerKey=suspend
      '';

    virtualboxHost.enable = true;

    logkeys = {
      enable = true;
      keymap = "pl";
    };

    lockX11Displays.enable = true;

    xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "ctrl:nocaps,compose:caps";
      synaptics = {
        enable = true;
        maxSpeed = "4.0";
        accelFactor = "0.02";
        twoFingerScroll = true;
        tapButtons = true;
        fingersMap = [1 3 2];
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

  fonts = {
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      corefonts
      hack-font
      inconsolata
      terminus_font
      unifont
      unifont_upper
    ];
  };

  users.extraUsers = {
    m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" ];
    };

  };

}
