{ flake, pkgs, lib, ... }:

{

  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    duplicity
    faad2
    ffmpeg-full
    flac
    flake.packages.${pkgs.system}.git-annex-hacks
    gitAndTools.gitRemoteGcrypt
    gitstats
    git-filter-repo
    gocr  # OCR
    #gpac  # marked as insecure
    gpgme.dev
    graphicsmagick
    imagemagick
    jhead
    lame
    normalize
    pdfgrep
    poppler_utils
    python3Packages.livereload
    (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
    shared-mime-info
    shellcheck
    shntool  # view and/or modify WAVE data and properties
    tesseract  # OCR
    x264
  ]);

}
