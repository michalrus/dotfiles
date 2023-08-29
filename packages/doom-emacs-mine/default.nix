{ doom-emacs }:

doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl     = ./init.el;
  doomConfigEl   = ./config.el;
  vendorHash     = "sha256-GbY7Mput88pvn1/tQjZA4pUsugjBeaLhnY9Pj7yxKKo=";
}
