{
  flake,
  pkgs,
  ...
}: let
  treefmt-wrapper =
    (flake.inputs.treefmt-nix.lib.mkWrapper pkgs {
      programs = {
        alejandra.enable = true; # Nix
        clang-format.enable = true; # Nix
        prettier.enable = true;
        ruff-check.enable = true; # Python
        ruff-format.enable = true; # Python
        rufo.enable = true; # Ruby
        rustfmt.enable = true;
        shfmt.enable = true;
        stylua.enable = true; # Lua
        taplo.enable = true; # TOML
        xmllint.enable = true; # XML
        yamlfmt.enable = true;
      };
    }).overrideAttrs (drv: {
      buildCommand =
        drv.buildCommand
        + ''
          chmod +w $out/bin/treefmt
          sed -r '/--tree-root-file=/ d' -i $out/bin/treefmt
        '';
    });
in {
  home-manager.sharedModules = [
    flake.packages.${pkgs.stdenv.hostPlatform.system}.opencode-bwrap.bwrap-escape-hatch.hmModule
    {
      services.bwrap-escape-hatch = {
        enable = true;
        rules = [
          {
            note = "Desktop notifications (any title + body)";
            argv = ["notify-send" "--" "*" "*"];
          }
        ];
      };
    }
    {
      home.packages = with pkgs; [
        git-filter-repo
        (python3.withPackages (p: with p; [scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf lz4 opencv4]))
        nixd
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
        flake.packages.${pkgs.stdenv.hostPlatform.system}.nixlint
        fd
        gh
        httpie # HTTP requests
        jq
        shellcheck
        treefmt-wrapper
        xh # HTTP requests
        yq # jq for YAML
        yamllint
        clang
        clang-tools # for clangd language server
        octave
        pandoc
        protobuf
        sqlint
        statix
      ];
      home.shellAliases.oc = "opencode-bwrap";
    }
  ];
}
