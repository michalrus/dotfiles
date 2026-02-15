{doom-emacs}:
doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl = ./init.el;
  doomConfigEl = ./config.el;
  vendorHash =
    {
      "x86_64-linux" = "sha256-tUqlOV5C/kPAtb9xS73D4a+uHTFhBUABz42EiZEh/60=";
      "aarch64-darwin" = ""; # FIXME
    }.${
      doom-emacs.system
    };
}
