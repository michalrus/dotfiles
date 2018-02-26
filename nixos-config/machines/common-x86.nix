{ config, lib, pkgs, ... }:

{
  imports = [
    ./common.nix
  ];

  nix = {
    trustedBinaryCaches = [
      http://hydra.nixos.org
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  environment.systemPackages = with pkgs; [
    (lowPrio gnupg1compat)
    (lowPrio stdmanpages)
    aspell
    aspellDicts.de
    aspellDicts.en
    aspellDicts.pl
    cloc
    duplicity
    exiv2
    faad2
    ffmpeg-full
    gitAndTools.gitRemoteGcrypt
    gitstats
    gnupg
    gocr
    graphicsmagick
    imagemagick
    imgurbash2
    indent
    jhead
    man_db
    manpages
    michalrus.git-annex
    michalrus.git-annex-desktop
    nix-prefetch-scripts
    nixos-unstable.catdocx
    normalize
    oathToolkit
    odt2txt
    pass
    poppler_utils
    posix_man_pages
    powertop
    #ripgrep  # TODO: why is it not on Hydra and wants to build rustc?
    shared_mime_info
    silver-searcher
    sox
    stdman
    tesseract
    unfree.nixos-unstable.geekbench
    wrk
  ];
}
