{ config, lib, pkgs, ... }:

{

  nix = {
    #useSandbox = true;   # not yet in stable!

    trustedBinaryCaches = [
      http://hydra.nixos.org
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];

    extraOptions = ''
      gc-keep-outputs = true
    '';
  };

  boot.tmpOnTmpfs = true;

  networking.firewall.rejectPackets = true;

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.enableCompletion = true;
  };

  services = {
    haveged.enable = true;

    locate = {
      enable = true;
      includeStore = true;
      interval = "hourly";
    };

    # Because of this insanity… → https://github.com/NixOS/nixpkgs/pull/16021
    logind.extraConfig = ''
      KillUserProcesses=yes
    '';
  };

  users = {
    mutableUsers = lib.mkDefault false;
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  environment.variables.PATH = [ "$HOME/.bin" ];

  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.pl
    aspellDicts.de
    bc
    bindfs
    calc
    cloc
    dos2unix
    exiv2
    file
    gcc
    git
    gitstats
    (lowPrio gnupg1compat)
    gnupg
    graphicsmagick
    htop
    hwinfo
    imagemagick
    #imgurbash2   # not yet in stable!
    indent
    jhead
    faad2   # video in Firefox
    ffmpeg
    gnumake
    #gnumake.doc  # not yet in stable!
    gocr
    lshw
    lsof
    ltrace
    man_db
    manpages
    mkpasswd
    moreutils
    mtr
    (hiPrio netcat-openbsd)
    nix-prefetch-scripts
    nix-repl
    nmap
    normalize
    openssl
    p7zip
    pciutils
    poppler_utils
    posix_man_pages
    powertop
    screen
    shared_mime_info
    silver-searcher
    socat
    sox
    sqlite
    (lowPrio stdmanpages)
    stdman
    strace
    tcp-broadcast
    telnet
    tesseract
    traceroute
    unzip
    urlwatch
    usbutils
    wget
    which
    whois
    wrk
    zip
  ];

  security = {
    setuidPrograms = [ "mtr" ];

    hideProcessInformation = true;

    sudo.extraConfig = ''
      Defaults timestamp_timeout=0
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.nix.package.out}/bin/nix-collect-garbage -d
    '';
  };

}
