{ flake, pkgs }:

let system = pkgs.stdenv.hostPlatform.system; in
let
  pkgs = flake.inputs.nixpkgs-unstable.legacyPackages.${system};

  # An override for a slightly newer revision:
  zed-editor-pre = pkgs.zed-editor.overrideAttrs (drv: rec {
    version = "0.216.0-pre";
    src = pkgs.fetchFromGitHub {
      owner = "zed-industries";
      repo = "zed";
      tag = "v${version}";
      hash = "sha256-0+0XcHnKOcWxxNbV/vg2bNkI1Xu7T+HxgCQPkdJ+0Xk=";
    };
    cargoHash = "sha256-0aKEbdV41ZNbMLx30DotmYDBuuxOgFOET79jjNhhd4Y="; # pkgs.lib.fakeHash; # "sha256-O4qH5FF2JIcy8NKvDPIfLF5tvdq8E0TNV0KlcWH6tFs=";
    cargoDeps = pkgs.rustPlatform.fetchCargoVendor {
      inherit src;
      hash = cargoHash;
      name = "${drv.pname}-${version}";
      inherit (drv.cargoDeps.vendorStaging) postBuild;
    };
  });

  # Fails with `EMFILE Too many open files`:
  zed-editor-flake = (
    builtins.getFlake "github:zed-industries/zed/e55a8c9cb1756058e653b3c2f65863dd10c46b58"; # v0.216.0-pre
  ).outputs.packages.${system}.default;
in
zed-editor-pre #zed-editor-flake
