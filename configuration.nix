{ config, pkgs, ... }:

{
  nix = {
    useChroot = true; # https://nixos.org/nixos/manual/options.html#opt-nix.useChroot

    trustedBinaryCaches = [
      http://hydra.nixos.org
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  imports =
    [
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

  networking = {
    hostName = "nixos";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ ];
    };
    #wireless.enable = true;       # wireless support via wpa_supplicant
    networkmanager.enable = true;  # wireless support via NetworkManager
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Warsaw";

  hardware = {
    sane.enable = true;
  };

  nixpkgs.config = {
    allowBroken = true;
    packageOverrides = pkgs: {
      youtube-dl = pkgs.callPackage "/etc/nixos/pkgs/youtube-dl-master.nix" {};
    };
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    cool-retro-term
    dmenu
    dos2unix
    dunst
    emacs
    file
    gcc
    git
    gnucash26
    gnupg1compat
    gnupg
    htop
    hwinfo
    i3lock
    i3status
    idea.idea-community
    imagemagick
    imgurbash
    indent
    isync
    faad2   # video in Firefox
    ffmpeg
    ffmpegthumbnailer
    gnumake
    libnotify
    #logkeys
    lshw
    lsof
    man_db
    manpages
    mkpasswd
    mpv
    mtr
    mu
    netcat-openbsd
    nmap
    oathToolkit
    openjdk7
    pass
    pciutils
    perlPackages.LWP
    pinentry
    pkgs.firefoxWrapper
    posix_man_pages
    rxvt_unicode-with-plugins
    screen
    shared_mime_info
    socat
    stdmanpages
    stdman
    transmission
    unzip
    usbutils
    wget
    which
    wmctrl
    xclip
    xdotool
    xsel
    youtube-dl
    zip
  ];

  programs = {
    zsh.enable = true;
    bash.enableCompletion = true;
  };

  services = {
    logind.extraConfig = ''
        HandleLidSwitch=suspend
        HandlePowerKey=suspend
      '';

    printing.enable = false;

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
    enableFontDir = true;
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

  users = {
    mutableUsers = false;
    defaultUserShell = "/run/current-system/sw/bin/zsh";

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "scanner" "networkmanager" ];
    };
  };

  security = {
    sudo.extraConfig = "Defaults timestamp_timeout=0";
    setuidPrograms = [ "mtr" ];
  };

}
