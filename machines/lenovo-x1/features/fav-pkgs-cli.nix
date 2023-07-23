{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    bat
    binutils
    gdb
    ipcalc
    gettext
    gist
    httrack
    libguestfs
    libxml2
    (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
    python3Packages.livereload
    tunctl
    watchexec
  ];

}
