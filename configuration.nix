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
    emacs
    file
    git
    gnome3.eog
    gnome3.gnome-font-viewer
    gnucash26
    gnupg1compat
    gnupg
    htop
    hwinfo
    imagemagick
    imgurbash
    isync
    faad2   # video in Firefox
    ffmpegthumbnailer
    libnotify
    #logkeys
    lshw
    lsof
    man_db
    mkpasswd
    mpv
    mtr
    mu
    nmap
    oathToolkit
    openjdk7
    pass
    pciutils
    pinentry
    pkgs.firefoxWrapper
    posix_man_pages
    screen
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
