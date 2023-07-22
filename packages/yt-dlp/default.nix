{ nixpkgs, yt-dlp, system }:

# XXX: it will return a newer one – either from nixpkgs or yt-dlp itself

let
  weAreNewer = yt-dlp.lastModified > nixpkgs.lastModified;
  base = nixpkgs.legacyPackages.${system}.yt-dlp.override { withAlias = true; };
in
if !weAreNewer
then base
else base.overridePythonAttrs (drv: {
  version = "nightly-" + yt-dlp.lastModifiedDate;
  src = yt-dlp;
})
