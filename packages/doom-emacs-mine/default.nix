{ doom-emacs }:

doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl     = ./init.el;
  doomConfigEl   = ./config.el;
  vendorHash     = "sha256-PeUAb+DiCBegnX8cogt09/CDs9nG5tqQ7pxnAyZoMK8=";
}
