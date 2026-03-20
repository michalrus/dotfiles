{
  pkgs,
  lib,
  bun2nix,
}: let
  # See <https://github.com/ex-machina-co/opencode-anthropic-auth/pull/13>:
  opencode-anthropic-auth = let
    src = pkgs.fetchFromGitHub {
      owner = "ex-machina-co";
      repo = "opencode-anthropic-auth";
      rev = "78ac824951ccb6e39c036f8ff3933dc23a877cea"; # pull/13/head
      hash = "sha256-VUhShn/Pg3SWW0xjc0TO4bVtlUilnadHbAs+CY3V6nc=";
    };
    # IFD: generate bun.nix from the upstream bun.lock using the bun2nix CLI.
    bun-nix = pkgs.runCommandLocal "opencode-anthropic-auth-bun.nix" {} ''
      ${lib.getExe bun2nix} --lock-file ${src}/bun.lock --output-file "$out"
    '';
  in
    pkgs.stdenv.mkDerivation rec {
      pname = "opencode-anthropic-auth";
      version = "0.0.20-pull.13";

      inherit src;

      nativeBuildInputs = [bun2nix.hook];

      bunDeps = bun2nix.fetchBunDeps {
        bunNix = bun-nix;
      };

      dontUseBunPatch = true;
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
        printf "\n\n// src: %s\n" ${src} >>"$out/dist/index.js"
        printf "\n\n// bun-nix: %s\n" ${bun-nix} >>"$out/dist/index.js"
        runHook postInstall
      '';
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

    patches = [./opencode-notifier-notify-send-double-dash.patch];

    nativeBuildInputs = [bun2nix.hook];

    bunDeps = bun2nix.fetchBunDeps {
      bunNix = opencode-notifier-bun-nix;
    };

    dontUseBunPatch = true;
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

  opencode-notifier-sounds = let
    macos-sounds = pkgs.fetchFromGitHub {
      owner = "extratone";
      repo = "macOSsystemsounds";
      rev = "f3e8dcd8d2318d099ade479ad1b9778ce4e65cc7";
      hash = "sha256-7Qa/MpYykTIOWkAhhoV1rrhScCkQKgcQAmmR38PdNRc=";
    };
  in
    pkgs.runCommand "macos-sounds-wav" {
      nativeBuildInputs = [pkgs.ffmpeg-headless];
    } ''
      mkdir -p "$out"
      ffmpeg -i "${macos-sounds}/m4r/Illuminate.m4r" "$out/Illuminate.wav"
      ffmpeg -i "${macos-sounds}/m4r/Chord.m4r" "$out/Chord.wav"
      ffmpeg -i "${macos-sounds}/aiff/Pop.aiff" "$out/Pop.wav"
      ffmpeg -i "${macos-sounds}/m4r/Hillside.m4r" "$out/Hillside.wav"
      ffmpeg -i "${macos-sounds}/aiff/Frog.aiff" "$out/Frog.wav"
    '';

  opencode-notifier-config = {
    showSessionTitle = true;
    messages = {
      permission = "{sessionTitle}\n→ needs permission";
      complete = "{sessionTitle}\n→ session finished";
      subagent_complete = "{sessionTitle}\n→ subagent completed";
      error = "{sessionTitle}\n→ error";
      question = "{sessionTitle}\n→ question(s)";
      user_cancelled = "{sessionTitle}\n→ cancelled by user";
    };
    sounds = {
      permission = "${opencode-notifier-sounds}/Illuminate.wav";
      complete = "${opencode-notifier-sounds}/Chord.wav";
      subagent_complete = "${opencode-notifier-sounds}/Pop.wav";
      error = "${opencode-notifier-sounds}/Hillside.wav";
      question = "${opencode-notifier-sounds}/Illuminate.wav";
      user_cancelled = "${opencode-notifier-sounds}/Frog.wav";
    };
  };

  opencode-plugins = pkgs.linkFarm "opencode-plugins" [
    {
      name = "opencode-anthropic-auth.js";
      path = "${opencode-anthropic-auth}/dist/index.js";
    }
    {
      name = "opencode-notifier.js";
      path = "${opencode-notifier}/dist/index.js";
    }
  ];
in {
  inherit
    opencode-anthropic-auth
    opencode-notifier
    opencode-notifier-config
    opencode-notifier-sounds
    opencode-plugins
    ;
}
