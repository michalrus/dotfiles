{ flake, system, lib, yt-dlp }:

# XXX: it will return a newer one – either from nixpkgs or yt-dlp itself
# – looking at versions; if versions are equal, it’ll take our input

let

  ours = flake.inputs.yt-dlp;
  theirs = yt-dlp.override { withAlias = true; };

  ourVersion = let
    file = __readFile (ours + "/yt_dlp/version.py");
    ms = __match ".*'([0-9]+\.[0-9]+\.[0-9]+)'.*" file;
  in if __isList ms && __length ms == 1 then __head ms else throw "Cannot determine version of ‘inputs.yt-dlp’";

  theyAreNewer = __compareVersions theirs.version ourVersion > 0;

in if theyAreNewer then theirs else theirs.overridePythonAttrs (drv: {
  src = ours;
  version = ourVersion + "-nightly-" + lib.substring 0 8 ours.lastModifiedDate;
})
