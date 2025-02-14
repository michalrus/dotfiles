{ pkgs }:

let

  flake-compat = pkgs.fetchFromGitHub rec {
    owner = "edolstra";
    repo = "flake-compat";
    rev = "v1.1.0";
    hash = "sha256-NeCCThCEP3eCl2l/+27kNNK7QrwZB1IJCrXfrbv5oqU=";
    name = "${repo}-${rev}";
  };

  cardano-node-src = pkgs.fetchFromGitHub rec {
    owner = "IntersectMBO";
    repo = "cardano-node";
    rev = "10.1.4";
    hash = "sha256-Oys38YkpSpB48/H2NseP9kTWXm92a7kjAZtdnorcIEY=";
    name = "${repo}-${rev}";
  };

  cardano-playground = pkgs.fetchFromGitHub rec {
    owner = "input-output-hk";
    repo = "cardano-playground";
    rev = "39ea4db0daa11d6334a55353f685e185765a619b"; # published as of 2025-02-13T10:32:15Z
    hash = "sha256-ORjEkhUGwRuxj3TiRDyplddsY9p27qg5Ah3lo9U8duw=";
    name = "${repo}-${rev}";
  };

  cardano-node-configs = builtins.path {
    name = "cardano-node-configs";
    path = cardano-playground + "/static/book.play.dev.cardano.org/environments";
  };

  cardano-node-configs--less-chatty = pkgs.runCommandNoCC "cardano-node-configs" {
    buildInputs = with pkgs; [ jq ];
  } ''
    cp -r ${cardano-node-configs} $out
    chmod -R +w $out
    find $out -name 'config.json' | while IFS= read -r configFile ; do
      jq '.
        | .TraceConnectionManagerCounters = false
        | .TracePeerSelection = false
        | .TracePeerSelectionActions = false
        | .TraceInboundGovernor = false
      ' "$configFile" >tmp.json
      mv tmp.json "$configFile"
    done
  '';

  mithril-src = pkgs.fetchFromGitHub rec {
    owner = "input-output-hk";
    repo = "mithril";
    rev = "2450.0";
    hash = "sha256-jT3sjtACWtiS1agD8XR6EKz73YpL0QelIS4RcBJy3F8=";
    name = "${repo}-${rev}";
  };

  blockfrost-platform-src = pkgs.fetchFromGitHub rec {
    owner = "blockfrost";
    repo = "blockfrost-platform";
    rev = "e6f4b483e2191439365e9ae215b3f89fea2423d8"; # `main` on 2025-02-12T16:48:07Z
    hash = "sha256-NZI2Ev3ifgodN594keNpwFCzxptz56mHqM+Q7syReNw=";
    name = "${repo}-${rev}";
  };

  cardano-node-flake = (import flake-compat { src = cardano-node-src.outPath; }).defaultNix;
  mithril-flake = (import flake-compat { src = mithril-src.outPath; }).defaultNix;
  blockfrost-platform-flake = (import flake-compat { src = blockfrost-platform-src.outPath; }).defaultNix;

  cardano-node = cardano-node-flake.packages.${pkgs.system}.cardano-node;
  cardano-cli = cardano-node-flake.packages.${pkgs.system}.cardano-cli;
  mithril-client = mithril-flake.packages.${pkgs.system}.mithril-client-cli;
  blockfrost-platform = blockfrost-platform-flake.packages.${pkgs.system}.default;

in

{

  inherit cardano-node cardano-cli cardano-node-configs cardano-node-configs--less-chatty mithril-client blockfrost-platform;

}
