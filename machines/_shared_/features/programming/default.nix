{ flake, config, lib, pkgs, ... }:

{
  home-manager.sharedModules = [{
    home.packages = with pkgs; [
      git-filter-repo
      (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf lz4 ]))
      nixd
      fpc
      ghc
      rustc
      rustfmt
      rust-analyzer
      cargo
      cargo-nextest
      flake.packages.${pkgs.stdenv.hostPlatform.system}.opencode-bwrap
      delta
      fd
      gh
      httpie # HTTP requests
      jq
      shellcheck
      shfmt
      xh # HTTP requests
      yq # jq for YAML
    ];
  }];
}
