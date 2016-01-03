{ config, pkgs, ... }:

{
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

  nixpkgs.config = {
    allowBroken = true;
  };

  hardware = {
    sane.enable = true;
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    dos2unix
    emacs
    file
    gcc
    git
    gnucash26
    gnupg1compat
    gnupg
    htop
    hwinfo
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
    xdotool
    zip
  ];

  programs = {
    zsh.enable = true;
    bash.enableCompletion = true;
  };

  services = {
    printing.enable = false;

    xserver = {
      enable = true;
      layout = "us";
      synaptics = {
        enable = true;
      };

      displayManager.gdm.enable = true;
      desktopManager.gnome3.enable = true;
    };
  };

  environment.gnome3.excludePackages = with pkgs.gnome3; [
    # apps
    accerciser bijiben evolution gnome-boxes gnome-calendar gnome-clocks
    gnome-documents gnome-getting-started-docs gnome-maps gnome-music
    gnome-photos gnome-weather polari vinagre
    # core
    empathy epiphany evolution_data_server folks gnome-calculator
    gnome-contacts gnome-dictionary gnome_online_accounts
    gnome-online-miners gnome-user-share totem-pl-parser totem
    tracker vino
  ];
  environment.gnome3.packageSet = pkgs.gnome3_18;

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
