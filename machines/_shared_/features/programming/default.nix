{ flake, config, lib, pkgs, ... }:

{
  home-manager.sharedModules = [{
    home.packages = with pkgs; [
      git-filter-repo
      (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf lz4 opencv4 ]))
      nixd
      alejandra
      fpc
      ghc
      gnumake
      cmake
      rustc
      rustfmt
      rust-analyzer
      cargo
      cargo-nextest
      clippy
      flake.packages.${pkgs.stdenv.hostPlatform.system}.opencode-bwrap
      fd
      gh
      httpie # HTTP requests
      jq
      shellcheck
      shfmt
      xh # HTTP requests
      yq # jq for YAML
      prettier
      yamlfmt
      yamllint
    ];
  }];
}
