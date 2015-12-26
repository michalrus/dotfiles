{ config, pkgs, ... }:

{
  imports =
    [
      ./hardware-configuration.nix
    ];

  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";
  boot.initrd.luks.devices  =  [ { name = "crypt"; device = "/dev/sda2"; } ];

  networking.hostName = "nixos";
  networking.nameservers = [ "8.8.8.8" "8.8.4.4" ];
  networking.firewall.enable = true;
  networking.firewall.allowPing = true;
  networking.firewall.allowedTCPPorts = [ ];
  #networking.wireless.enable = true;       # wireless support via wpa_supplicant
  networking.networkmanager.enable = true;  # wireless support via NetworkManager

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Warsaw";

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    emacs
    file
    git
    gnupg
    htop
    imagemagick
    lsof
    man_db
    mpv
    mu
    nmap
    pass
    pkgs.firefoxWrapper
    posix_man_pages
    pthreadmanpages
    screen
    stdmanpages
    stdman
    transmission
    unzip
    wget
    zip
  ];

  programs.zsh.enable = true;
  programs.bash.enableCompletion = true;

  services.printing.enable = false;

  services.xserver.enable = true;
  services.xserver.layout = "us";
  # services.xserver.xkbOptions = "eurosign:e";
  services.xserver.synaptics.enable = true;

  services.xserver.displayManager.gdm.enable = true;
  services.xserver.desktopManager.gnome3.enable = true;
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

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  users.extraUsers.m = {
    isNormalUser = true;
    uid = 1000;
    description = "Michal Rus";
    extraGroups = [ "wheel" ];
  };

  security.sudo.extraConfig = "Defaults timestamp_timeout=0";

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "15.09";

}
