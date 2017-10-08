{ config, lib, pkgs, ... }:

# See https://github.com/NixOS/nixpkgs/issues/28815#issuecomment-335010790

{

  system.replaceRuntimeDependencies = with pkgs; [
    { original = aspell;
      replacement = aspell.overrideAttrs (oldAttrs: rec {
        patches = [ ./aspell-fix.patch ];
        # patchPhase = oldAttrs.patchPhase + ''
        #   patch -p1 < ${./aspell-fix.patch}
        # '';
      });
   }
  ];

}
