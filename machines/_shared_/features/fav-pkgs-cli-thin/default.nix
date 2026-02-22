{
  flake,
  lib,
  pkgs,
  ...
}: {
  environment.systemPackages =
    flake.lib.filterSystem pkgs.stdenv.hostPlatform.system (with pkgs; [
      (lib.hiPrio arping)
      (lib.hiPrio netcat-openbsd)
      (lib.lowPrio gnupg1compat)
      (lib.lowPrio stdmanpages)
      arpoison
      aspell
      aspellDicts.de
      aspellDicts.en
      aspellDicts.pl
      bat
      bc
      bindfs
      binutils
      bottom # `bin/btm`, an `htop` alternative
      calc
      catdocx
      cloc
      conntrack-tools
      daemonize
      ddrescue
      dos2unix
      dvdbackup
      easyrsa
      entr
      evemu
      evtest
      exiv2
      ext4magic
      file
      (flake.inputs.agenix.packages.${pkgs.stdenv.hostPlatform.system}.default.overrideAttrs (drv: {
        # Produce ASCII-armored output instead of binary files:
        installPhase =
          (drv.installPhase or "")
          + ''
            sed -r 's/rage (.*ENCRYPT)/rage --armor \1/g' -i $out/bin/agenix
          '';
      }))
      gcc
      gdb
      gettext
      gist
      git
      gnumake
      gnupg
      hdparm
      htop
      btop
      httperf
      httrack
      hwinfo
      iftop
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
      lz4
      (
        if lib.versionAtLeast lib.version "23.04"
        then man-db
        else man_db
      )
      (
        if lib.versionAtLeast lib.version "23.04"
        then man-pages
        else manpages
      )
      mkpasswd
      moreutils
      ncdu
      nix-prefetch-scripts
      nmap
      ntfs3g
      oath-toolkit
      odt2txt
      openssl
      p7zip
      pass
      patchelf
      pciutils
      man-pages-posix
      powertop
      pv
      safecopy
      silver-searcher
      smartmontools
      socat
      speedtest-cli
      sqlite-interactive
      stdman
      strace
      stress-ng
      tcpdump
      tree
      tunctl
      unixtools.xxd
      unzip
      usbutils
      watchexec
      wget
      which
      wireguard-tools
      wirelesstools
      wrk
      zip
    ])
    # FIXME: these have wrong meta.platforms:
    ++ (
      if pkgs.stdenv.hostPlatform.system == "x86_64-linux"
      then
        with pkgs; [
          libguestfs
        ]
      else []
    )
    # FIXME: these don’t build on aarch64-darwin in nixos-2003
    ++ (
      if pkgs.stdenv.hostPlatform.system != "aarch64-linux"
      then
        with pkgs; [
          ripgrep
        ]
      else []
    );
}
