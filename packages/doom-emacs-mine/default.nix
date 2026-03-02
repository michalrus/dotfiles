{doom-emacs}:
doom-emacs.override {
  doomPackagesEl = ./packages.el;
  doomInitEl = ./init.el;
  doomConfigEl = ./config.el;
  vendorHash =
    {
      "x86_64-linux" = "sha256-uolaZFpEdPRe8kubaD25jf6a/J8hMdts+LqkZUGJSsw=";
      "aarch64-darwin" = ""; # FIXME
    }.${
      doom-emacs.system
    };
}
