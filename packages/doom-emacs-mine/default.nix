{doom-emacs}:
doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl = ./init.el;
  doomConfigEl = ./config.el;
  vendorHash =
    {
      "x86_64-linux" = "sha256-i5oO6NSmYh+5QMgqf6itcFw/jZMVUZaaDw9RYwbAauY=";
      "aarch64-darwin" = ""; # FIXME
    }.${
      doom-emacs.system
    };
}
