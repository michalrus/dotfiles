{ flake, config, lib, pkgs, ... }:

{
  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    ripgrep
    (lowPrio gnupg1compat)
    (lowPrio stdmanpages)
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.pl
    catdocx
    cloc
    duplicity
    exiv2
    faad2
    ffmpeg-full
    gitAndTools.gitRemoteGcrypt
    gitstats
    gnupg
    gocr
    gpgme.dev
    graphicsmagick
    imagemagick
    imgurbash2
    indent
    jhead
    ltrace
    man_db
    manpages
    flake.packages.${pkgs.system}.git-annex-hacks
    nix-prefetch-scripts
    normalize
    oathToolkit
    odt2txt
    pass
    poppler_utils
    posix_man_pages
    powertop
    shared_mime_info
    silver-searcher
    sox
    speedtest-cli
    stdman
    tesseract
    wrk
  ]);
}
