{doom-emacs}:
doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl = ./init.el;
  doomConfigEl = ./config.el;
  vendorHash =
    {
      "x86_64-linux" = "sha256-IMD890+lh1NAnf6qXkmluULKOXCuORMKMiZ0DOlZtyQ=";
      "aarch64-darwin" = ""; # FIXME
    }.${
      doom-emacs.system
    };
}
