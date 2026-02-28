{
  pkgs,
  lib,
  bun2nix,
}: let
  opencode-md-table-formatter = pkgs.fetchFromGitHub {
    owner = "franlol";
    repo = "opencode-md-table-formatter";
    tag = "v0.0.6";
    hash = "sha256-cmLsPeUnGo1spaz1UGhIYPdmIdRnLQ3tEaONoMGBTcw=";
  };

  opencode-notifier-src = pkgs.fetchFromGitHub {
    owner = "mohak34";
    repo = "opencode-notifier";
    tag = "v0.1.28";
    hash = "sha256-Ne4X4q5LidbHax9HZu22qk/jQg2k+aA09c9E7cs4KR4=";
  };

  # IFD: generate bun.nix from the upstream bun.lock using the bun2nix CLI.
  opencode-notifier-bun-nix = pkgs.runCommandLocal "opencode-notifier-bun.nix" {} ''
    ${lib.getExe bun2nix} --lock-file ${opencode-notifier-src}/bun.lock --output-file "$out"
  '';

  opencode-notifier = pkgs.stdenv.mkDerivation {
    pname = "opencode-notifier";
    version = "0.1.28";

    src = opencode-notifier-src;

    nativeBuildInputs = [bun2nix.hook];

    bunDeps = bun2nix.fetchBunDeps {
      bunNix = opencode-notifier-bun-nix;
    };

    dontUseBunBuild = true;
    dontUseBunCheck = true;
    dontUseBunInstall = true;

    buildPhase = ''
      runHook preBuild
      bun build src/index.ts --outdir dist --target node
      runHook postBuild
    '';

    installPhase = ''
      runHook preInstall
      mkdir -p "$out/dist"
      cp dist/index.js "$out/dist/index.js"
      cp -r sounds "$out/sounds"
      cp -r logos "$out/logos"
      runHook postInstall
    '';
  };

  opencode-plugins = pkgs.linkFarm "opencode-plugins" [
    {
      name = "opencode-md-table-formatter.ts";
      path = "${opencode-md-table-formatter}/index.ts";
    }
    {
      name = "opencode-notifier.js";
      path = "${opencode-notifier}/dist/index.js";
    }
  ];
in {
  inherit
    opencode-md-table-formatter
    opencode-notifier
    opencode-plugins
    ;
}
