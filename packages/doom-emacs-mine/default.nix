{doom-emacs}:
doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl = ./init.el;
  doomConfigEl = ./config.el;
  vendorHash =
    {
      "x86_64-linux" = "sha256-ymKiTjEe8BQQs6W+aU6ZkpFFahdCV5r5i1T3atXYjUs=";
      "aarch64-darwin" = ""; # FIXME
    }.${
      doom-emacs.system
    };
}
