{ doom-emacs }:

doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl     = ./init.el;
  doomConfigEl   = ./config.el;
  vendorHash     = "sha256-CFo0jZgvTgBvg6dUy8ZY13gNwGGVwAbkaZ1abWsyUo0=";
}
