{ flake, config, lib, pkgs, ... }:

{
  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    (lowPrio gnupg1compat)
    (lowPrio stdmanpages)
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.pl
    bat
    binutils
    catdocx
    cloc
    exiv2
    gdb
    gettext
    gist
    gnupg
    httrack
    imgurbash2
    indent
    ipcalc
    libjpeg
    libxml2
    ltrace
    man_db
    manpages
    nix-prefetch-scripts
    ntfs3g
    oathToolkit
    odt2txt
    pass
    posix_man_pages
    powertop
    ripgrep
    silver-searcher
    speedtest-cli
    stdman
    tunctl
    watchexec
    wrk
  ])

  # FIXME: these have wrong meta.platforms:
  ++ (if pkgs.system == "x86_64-linux" then with pkgs; [

    libguestfs

  ] else []);
}
