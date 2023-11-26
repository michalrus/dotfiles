{ doom-emacs }:

doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl     = ./init.el;
  doomConfigEl   = ./config.el;
  vendorHash     = "sha256-hIkcRfVEJuPYjcFZ0oNZ8MjTCcboiF2M3P9P60Lnajk=";
}
