{doom-emacs}:
doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl = ./init.el;
  doomConfigEl = ./config.el;
  vendorHash =
    {
      "x86_64-linux" = "sha256-OLDZOnA7VcQ07SpTBHA5FWejMk7BeEls2WaS+qKx2SA=";
      "aarch64-darwin" = ""; # FIXME
    }.${
      doom-emacs.system
    };
}
