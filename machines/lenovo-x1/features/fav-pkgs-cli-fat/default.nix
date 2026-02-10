{ flake, pkgs, lib, ... }:

{

  environment.systemPackages = flake.lib.filterSystem pkgs.stdenv.hostPlatform.system (with pkgs; [
    duplicity
    faad2
    flac
    #flake.packages.${pkgs.stdenv.hostPlatform.system}.git-annex-hacks
    #gitAndTools.gitRemoteGcrypt
    gitstats
    gocr  # OCR
    #gpac  # marked as insecure
    gpgme.dev
    jhead
    lame
    normalize
    pdfgrep
    poppler-utils
    unpaper
    img2pdf
    python3Packages.livereload
    shared-mime-info
    shellcheck
    shntool  # view and/or modify WAVE data and properties
    tesseract  # OCR
    x264
  ]);

}
