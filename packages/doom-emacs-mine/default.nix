{ doom-emacs }:

doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl     = ./init.el;
  doomConfigEl   = ./config.el;
  vendorHash     = "sha256-dAuhQK5JlHQruqZOyFHsyjGXeslI2dDOYHlfEkfiExY=";
}
