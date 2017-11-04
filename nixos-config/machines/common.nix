{ config, lib, pkgs, ... }:

{
  nix = {
    trustedBinaryCaches = [
      http://hydra.nixos.org
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];

    extraOptions = ''
      gc-keep-outputs = true
      build-cache-failure = true
      auto-optimise-store = true
    '';

    useSandbox = true;
  };

  networking.firewall.rejectPackets = true;

  programs = {
    mtr.enable = true;
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.enableCompletion = true;
  };

  services = {
    haveged.enable = true;

    smartd.enable = lib.mkDefault true;

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
    (hiPrio netcat-openbsd)
    (lowPrio gnupg1compat)
    (lowPrio stdmanpages)
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.pl
    bc
    bindfs
    calc
    cloc
    daemonize
    ddrescue
    dos2unix
    duplicity
    easyrsa
    exiv2
    faad2
    ffmpeg
    file
    gcc
    git
    gitAndTools.gitRemoteGcrypt
    gitstats
    gnumake
    gnupg
    gocr
    graphicsmagick
    htop
    hwinfo
    imagemagick
    imgurbash2
    indent
    inetutils
    jhead
    jq
    libfaketime
    lshw
    lsof
    ltrace
    man_db
    manpages
    michalrus.catdocx
    michalrus.git-annex
    michalrus.git-annex-desktop
    michalrus.tcp-broadcast
    mkpasswd
    moreutils
    mtr
    nix-prefetch-scripts
    nix-repl
    nixos-unstable.geekbench
    nmap
    normalize
    oathToolkit
    odt2txt
    openssl
    p7zip
    pass
    pciutils
    poppler_utils
    posix_man_pages
    powertop
    ripgrep
    shared_mime_info
    silver-searcher
    smartmontools
    socat
    sox
    sqlite-interactive
    stdman
    strace
    tesseract
    unzip
    urlwatch
    usbutils
    wget
    which
    wrk
    zip
  ];

  security = {
    pam.services.su.requireWheel = true;

    hideProcessInformation = true;

    sudo.extraConfig = ''
      Defaults timestamp_timeout=0
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch -k
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch -k --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot -k
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot -k --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.nix.package.out}/bin/nix-collect-garbage -d
    '';
  };

  # Stability!
  system.autoUpgrade.enable = lib.mkDefault false;
}
