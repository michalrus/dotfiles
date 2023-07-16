{ inputs, system }:

let
  nixpkgs = inputs.nixpkgs;
  pkgs = nixpkgs.legacyPackages.${system};
  weAreNewer = inputs.yt-dlp.lastModified > nixpkgs.lastModified;
  base = pkgs.yt-dlp.override { withAlias = true; };
in
if !weAreNewer
then base
else base.overridePythonAttrs (drv: {
  version = "nightly-" + inputs.yt-dlp.lastModifiedDate;
  src = inputs.yt-dlp;
})
