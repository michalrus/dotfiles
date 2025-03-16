{ flake, pkgs, lib, ... }:

{

  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    duplicity
    faad2
    flac
    flake.packages.${pkgs.system}.git-annex-hacks
    gitAndTools.gitRemoteGcrypt
    gitstats
    gocr  # OCR
    #gpac  # marked as insecure
    gpgme.dev
    jhead
    lame
    normalize
    pdfgrep
    poppler_utils
    python3Packages.livereload
    shared-mime-info
    shellcheck
    shntool  # view and/or modify WAVE data and properties
    tesseract  # OCR
    x264
  ]);

}
