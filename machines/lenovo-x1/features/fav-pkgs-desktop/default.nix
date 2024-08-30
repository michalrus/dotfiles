{ flake, config, pkgs, ... }:

let
  unfree = import pkgs.path { inherit (pkgs) system; config.allowUnfree = true; };
in

{

  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };

  boot.kernel.sysctl."fs.inotify.max_user_watches" = "1048576";  # for JetBrains

  # Use GTK 2 in LibreOffice, as 3 has some menu rendering problems.
  environment.variables."SAL_USE_VCLPLUGIN" = "gtk";

  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    haskellPackages.ghc
    haskellPackages.hlint
    jetbrains.idea-community
    nodejs_latest
    octave
    openjdk8
    pandoc
    protobuf
    rust-analyzer
    rustup
    solc
    sqlint
    vscodium
    (texlive.withPackages (ps: [
      flake.packages.${pkgs.system}.gregorio.forTexlive
    ] ++ (with ps; [
      scheme-small latexmk titlesec tocloft todonotes cleveref lipsum
      biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski
      placeins xstring pdfpages unicode-math filehook textpos marvosym fontawesome
      progressbar lm-math ucharcat pdfjam
      # for Org-mode export to PDF
      wrapfig wasysym
    ])))
    acpitool
    aegisub
    alacritty
    alsaUtils
    anki
    audacity
    awf
    breeze-qt5 breeze-icons hicolor-icon-theme kde-gtk-config breeze-gtk
    brightnessctl
    calibre
    cdparanoia
    cdrkit
    clang_16
    clang-tools # for clangd language server
    cool-retro-term
    devede
    diff-pdf
    dvdauthor
    dvdbackup
    electrum
    evince
    feh
    flake.packages.${pkgs.system}.gettext-emacs
    flake.packages.${pkgs.system}.noise
    flake.packages.${pkgs.system}.transcribe
    ghostscript
    gimp
    gnome3.adwaita-icon-theme  # for resizable cursors
    gnome3.aisleriot
    gnome3.baobab
    gnome3.cheese
    dconf  # so that GnuCash prefs can be changed
    gnome3.zenity
    gparted
    gtk2  # Why? Icon cache! See #20874.
    handbrake
    inkscape
    isync
    k3b
    libnotify
    libreoffice
    lilypond
    monero-gui
    networkmanagerapplet
    octave
    pavucontrol
    pcmanfm
    pdfarranger
    pdfpc
    pinentry-gtk2
    flake.inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.chromium
    gnucash
    #unfree.retroarchFull
    streamlink
    qjoypad
    rpcs3
    rtmpdump
    samba
    scantailor
    simple-scan
    speedread
    statix
    tigervnc
    timidity
    unfree.skypeforlinux  # FIXME: use inside browser?
    unfree.zoom-us  # FIXME: use inside browser?
    utox
    xarchiver
    xdg_utils
    xsane
  ]);

}
