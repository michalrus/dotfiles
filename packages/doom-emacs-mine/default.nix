{ doom-emacs }:

doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl     = ./init.el;
  doomConfigEl   = ./config.el;
  vendorHash     = {
    "x86_64-linux" = "sha256-PeUAb+DiCBegnX8cogt09/CDs9nG5tqQ7pxnAyZoMK8=";
    "aarch64-darwin" = "sha256-oRlC4bsT1IHicUBXZLBPM0ooMes/mkRKpfMZR/TxGoY=";
  }.${doom-emacs.system};
}
