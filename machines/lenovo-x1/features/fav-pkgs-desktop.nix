{ flake, config, pkgs, ... }:

let
  pkgs-23_05 = flake.inputs.nixpkgs.legacyPackages.${pkgs.system};
  unfree-23_05 = import flake.inputs.nixpkgs { inherit (pkgs) system; config.allowUnfree = true; };
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
  ];

  boot.kernel.sysctl."fs.inotify.max_user_watches" = "1048576";  # for JetBrains

  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithHoogle (hs: []))
    haskellPackages.hlint
    rustup
    rust-analyzer
    sqlint
    openjdk8
    jetbrains.idea-community
    vscodium
    octave
    pandoc
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
    protobuf

    (wine.override { pulseaudioSupport = true; })
    acpitool
    aegisub
    alacritty
    anki
    breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    brightnessctl
    cool-retro-term
    dvdbackup
    feh
    ghostscript
    gnome3.adwaita-icon-theme # for resizable cursors
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.zenity
    k3b
    flake.packages.${pkgs.system}.gettext-emacs
    flake.packages.${pkgs.system}.noise
    networkmanagerapplet
    pdfpc
    qjoypad
    pkgs-23_05.retroarchFull
    rpcs3
    termite
    tigervnc
    flake.packages.${pkgs.system}.transcribe
    speedread
    xdg_utils

    pkgs-23_05.chromium
    electrum
    pkgs-23_05.gnucash
    isync
    unfree-23_05.skypeforlinux  # FIXME: use inside browser?
    unfree-23_05.zoom-us  # FIXME: use inside browser?
    lilypond
    monero-gui
  ];

}
