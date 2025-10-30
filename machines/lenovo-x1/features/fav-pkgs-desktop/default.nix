{ flake, config, pkgs, ... }:

let
  unfree = import pkgs.path { inherit (pkgs) system; config.allowUnfree = true; };
in

{

  programs.wireshark = {
    enable = true;
    package = pkgs.wireshark-qt;
  };

  environment.systemPackages = flake.lib.filterSystem pkgs.system (with pkgs; [
    haskellPackages.ghc
    haskellPackages.hlint
    octave
    pandoc
    protobuf
    rust-analyzer
    cargo-nextest
    rustup
    sqlint
    (texlive.withPackages (ps: [
      flake.packages.${pkgs.system}.gregorio.forTexlive
    ] ++ (with ps; [
      scheme-small latexmk titlesec tocloft todonotes cleveref lipsum
      biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski
      placeins xstring pdfpages unicode-math filehook textpos marvosym fontawesome
      progressbar lm-math ucharcat pdfjam
      enumitem
      # for Org-mode export to PDF
      wrapfig wasysym
      # chemistry:
      chemfig
      chemformula
      mhchem
      chemmacros translations elements relsize chemnum siunitx
    ])))
    acpitool
    aegisub
    alsa-utils
    flake.inputs.nixpkgs-unstable.legacyPackages.${pkgs.system}.anki
    audacity
    awf
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
    flake.packages.${pkgs.system}.gettext-emacs
    flake.packages.${pkgs.system}.noise
    flake.packages.${pkgs.system}.transcribe
    ghostscript
    gimp
    aisleriot
    baobab
    cheese
    zenity
    gparted
    handbrake
    inkscape
    isync
    #k3b
    lilypond
    monero-gui
    octave
    pdfarranger
    pdfpc
    pdftk
    qpdf
    ocrmypdf
    exiftool
    pinentry-gtk2
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
    # unfree.skypeforlinux  # FIXME: use inside browser?
    # unfree.zoom-us  # FIXME: use inside browser?
    utox
    xsane
    gfortran
    fortls
  ]);

}
