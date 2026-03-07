{
  pkgs,
  lib,
  ...
}: let
  unfree = import pkgs.path {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };
  retroarch-full = unfree.retroarch.withCores (
    cores:
      lib.filter (
        c:
          (c ? libretroCore)
          && (lib.meta.availableOn pkgs.stdenv.hostPlatform c)
          && (c.core or "" != "fbalpha2012") # broken on current 25.11
      ) (lib.attrValues cores)
  );
in {
  environment.systemPackages = with pkgs; [
    aisleriot
    retroarch-full
  ];
}
