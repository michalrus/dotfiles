{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../pkgs
    ../../local
    ../../common.nix
    ../../hardware-configuration.nix
    ../../boot.nix
  ];

  nix.useSandbox = true;   # move to common.nix when in stable!

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
    firewall.nonetGroup.enable = true;
    connman = {
      enable = true;
      # https://wiki.archlinux.org/index.php/Connman#Avoid_changing_the_hostname
      extraConfig = ''
        [General]
        AllowHostnameUpdates=false
      '';
    };
  };

  time.timeZone = "Europe/Warsaw";

  hardware = {
    sane.enable = true;
    sane.extraConfig.pixma = "bjnp://10.0.1.5";
    opengl.driSupport32Bit = true; # for Wine
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

  nixpkgs.config = {
    allowBroken = true;
  };

  environment.systemPackages = with pkgs; [
    arandr
    awf-gtk
    calibre
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
    gimp
    gnome.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnucash26
    gnumake.doc  # move to common.nix when in stable!
    gparted
    # (haskellPackages.ghcWithHoogle (haskellPackages: with haskellPackages; [
    (haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [
      cabal-install happy hindent hlint parallel stylish-haskell turtle
      # ghc-mod hasktags
    ]))
    imgurbash2   # move to common.nix when in stable!
    isync
    jmeter
    jmtpfs
    libmtp
    libnotify
    lilypond
    mpv
    mu
    oathToolkit
    openjdk8
    pass
    pavucontrol
    pinentry
    python34Packages.livestreamer
    rtmpdump
    scala
    st
    (texlive.combine {
      inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages;
      gregorio = pkgs.gregorio.forTexlive;
    })
    transcribe
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
    xfe
    xorg.xbacklight
    xorg.xev
    xorg.xdpyinfo
    xorg.xmodmap
    xrandr-invert-colors
    xsane
    xsel
  ];

  environment.shellInit = ''
    export GTK2_RC_FILES=${pkgs.gnome3.gnome_themes_standard}/share/themes/Adwaita/gtk-2.0/gtkrc
    '';

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

    logkeys = {
      enable = true;
      keymap = "pl";
    };

    lockX11Displays.enable = true;

    printing = {
      enable = true;
      gutenprint = true;
    };

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
      hack-font
      inconsolata
      terminus_font
      unifont
      unifont_upper
    ];
  };

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

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
