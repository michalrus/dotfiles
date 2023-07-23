{ flake, pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    bat
    binutils
    flac
    flake.packages.${pkgs.system}.yt-dlp
    gdb
    gettext
    gist
    gpac
    httrack
    ipcalc
    lame
    libguestfs
    libjpeg
    libxml2
    ntfs3g
    pdfgrep
    (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
    python3Packages.livereload
    shellcheck
    shntool
    tunctl
    watchexec
    x264
  ];

}
