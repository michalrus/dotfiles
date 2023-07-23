{ flake, config, pkgs, ... }:

let
  pkgs-2305 = flake.inputs.nixpkgs-2305.legacyPackages.${pkgs.system};
  unfree-2305 = import flake.inputs.nixpkgs-2305 { inherit (pkgs) system; config.allowUnfree = true; };
in

{

  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };

  fonts.fonts = with pkgs; [
    anonymousPro
    hack-font
    iosevka-bin
    font-awesome-ttf
    font-awesome
    google-fonts
    terminus_font
    unifont
    unifont_upper
  ];

  fonts.enableGhostscriptFonts = true;

  boot.kernel.sysctl."fs.inotify.max_user_watches" = "1048576";  # for JetBrains

  # Use GTK 2 in LibreOffice, as 3 has some menu rendering problems.
  environment.variables."SAL_USE_VCLPLUGIN" = "gtk";

  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    haskellPackages.ghc
    haskellPackages.hlint
    jetbrains.idea-community
    octave
    openjdk8
    pandoc
    protobuf
    rust-analyzer
    rustup
    sqlint
    vscodium
    (texlive.combine {
      inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum
        biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski
        placeins xstring pdfpages unicode-math filehook textpos marvosym fontawesome
        progressbar lm-math ucharcat pdfjam
        # for Org-mode export to PDF
        wrapfig wasysym
        ;
      gregorio = flake.packages.${pkgs.system}.gregorio.forTexlive;
    })

    acpitool
    aegisub
    alacritty
    alsaUtils
    anki
    audacity
    awf
    breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    brightnessctl
    calibre
    cdparanoia
    cdrkit
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
    gnome3.adwaita-icon-theme # for resizable cursors
    gnome3.aisleriot
    gnome3.baobab
    gnome3.cheese
    gnome3.dconf   # so that GnuCash prefs can be changed
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
    mpv
    networkmanagerapplet
    octave
    pavucontrol
    pcmanfm
    pdfarranger
    pdfpc
    pinentry-gtk2
    pkgs-2305.chromium
    pkgs-2305.gnucash
    pkgs-2305.retroarchFull
    python3Packages.livestreamer
    qjoypad
    rpcs3
    rtmpdump
    samba
    scantailor
    simple-scan
    speedread
    termite
    tigervnc
    timidity
    unfree-2305.skypeforlinux  # FIXME: use inside browser?
    unfree-2305.zoom-us  # FIXME: use inside browser?
    utox
    xarchiver
    xdg_utils
    xsane
  ]);

}
