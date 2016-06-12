{ config, pkgs, ... }:

{

  nix = {
    useSandbox = true;

    trustedBinaryCaches = [
      http://hydra.nixos.org
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  boot.tmpOnTmpfs = true;

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.enableCompletion = true;
  };

  services = {
    haveged.enable = true;
    printing.enable = false;
    locate = {
      enable = true;
      includeStore = true;
      interval = "hourly";
    };
  };

  users = {
    mutableUsers = false;
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  environment.systemPackages = with pkgs; [
    aspell
    aspellDicts.en
    aspellDicts.pl
    aspellDicts.de
    bc
    calc
    cloc
    dos2unix
    exiv2
    file
    gcc
    git
    gitstats
    gnupg1compat
    gnupg
    gpac
    graphicsmagick
    htop
    hwinfo
    imagemagick
    imgurbash2
    indent
    jhead
    faad2   # video in Firefox
    ffmpeg
    gnumake
    gocr
    libjpeg
    lshw
    lsof
    man_db
    manpages
    mkpasswd
    moreutils
    mtr
    netcat-openbsd
    nix-prefetch-scripts
    nix-repl
    nmap
    normalize
    p7zip
    pciutils
    perlPackages.ConfigTiny
    perlPackages.DateCalc
    perlPackages.JSON
    perlPackages.LWP
    perlPackages.LWPProtocolHttps
    poppler_utils
    posix_man_pages
    screen
    shared_mime_info
    silver-searcher
    socat
    sox
    stdmanpages
    stdman
    tesseract
    unzip
    urlwatch
    usbutils
    wget
    which
    whois
    wrk
    x264
    youtube-dl
    zip
  ];

  security = {
    setuidPrograms = [ "mtr" ];

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
