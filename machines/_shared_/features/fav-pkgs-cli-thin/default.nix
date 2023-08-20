{ flake, config, lib, pkgs, ... }:

{
  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    (hiPrio arping)
    (hiPrio netcat-openbsd)
    (lowPrio gnupg1compat)
    (lowPrio stdmanpages)
    arpoison
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.pl
    bat
    bc
    bindfs
    binutils
    calc
    catdocx
    cloc
    conntrack-tools
    daemonize
    ddrescue
    dos2unix
    easyrsa
    entr
    evemu
    evtest
    exiv2
    ext4magic
    file
    gcc
    gdb
    gettext
    gist
    git
    gnumake
    gnupg
    hdparm
    htop
    httperf
    httrack
    hwinfo
    imgurbash2
    indent
    inetutils
    inotify-tools
    iotop
    ipcalc
    iw
    jq
    libfaketime
    libjpeg
    libxml2
    lm_sensors
    lshw
    lsof
    ltrace
    (if lib.versionAtLeast lib.version "23.04" then man-db else man_db)
    (if lib.versionAtLeast lib.version "23.04" then man-pages else manpages)
    mkpasswd
    moreutils
    ncdu
    nix-prefetch-scripts
    nmap
    ntfs3g
    oathToolkit
    odt2txt
    openssl
    p7zip
    pass
    pciutils
    posix_man_pages
    powertop
    pv
    silver-searcher
    smartmontools
    socat
    speedtest-cli
    sqlite-interactive
    stdman
    strace
    stress-ng
    tcpdump
    tunctl
    unzip
    usbutils
    watchexec
    wget
    which
    wirelesstools
    wrk
    zip
  ])

  # FIXME: these have wrong meta.platforms:
  ++ (if pkgs.system == "x86_64-linux" then with pkgs; [

    libguestfs

  ] else [])

  # FIXME: these don’t build on aarch64-darwin in nixos-2003
  ++ (if pkgs.system != "aarch64-linux" then with pkgs; [

    ripgrep

  ] else []);
}
