{ pkgs, runCommandNoCC, callPackage }:

let
  orig = pkgs.path + "/pkgs/development/libraries/hunspell/dictionaries-chromium.nix";
  patched = pkgs.runCommandNoCC "patched" {} ''
    sed '$s/.*/  inherit mkDictFromChromium; }/' <${orig} >$out
  '';
  inherit (callPackage patched {}) mkDictFromChromium;
in

# <https://chromium.googlesource.com/chromium/deps/hunspell_dictionaries/+/refs/heads/main>
mkDictFromChromium {
  shortName = "pl-pl";
  dictFileName = "pl-PL-3-0.bdic";
  shortDescription = "Polish (Poland)";
}
