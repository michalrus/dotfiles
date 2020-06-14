{ config, lib, pkgs, ... }:

with lib;

let

  etcDirName = "profiles/dynamic";
  loadFile = "load.include.sh";

  immutableProfile = name: profileConfig: let
    pname = "dynamic-profile-${name}";
    doLoad = pkgs.writeText "load-${pname}" (
      exportDynamicProfiles [ "/etc/${etcDirName}/${name}" ]
      + "\n" +
      profileConfig.extraSetup
    );
  in pkgs.buildEnv {
    name = pname;
    paths = profileConfig.packages;
    inherit (config.environment) pathsToLink extraOutputsToInstall;
    inherit (config.system.path) ignoreCollisions;
    postBuild = config.system.path.postBuild + ''
      ln -s ${doLoad} $out/${loadFile}
      ${profileConfig.extraPostBuild}
    '';
  };

  #
  # Actual environment setup.
  #
  # Adapted from <https://github.com/NixOS/nixpkgs/blob/0c3a28f08f207a0b074d5ecbbed21b5b045f690f/nixos/modules/config/shells-environment.nix#L12-L28>.
  #
  exportDynamicProfiles = dynProfiles: with pkgs.lib; let
    cfg = config.environment;
    suffixedVariables = flip mapAttrs cfg.profileRelativeEnvVars (envVar: listSuffixes: concatMap (
      profile: map (suffix: "${profile}${suffix}") listSuffixes) dynProfiles
    );
    exportVariables = mapAttrsToList (n: vs:
      concatMapStringsSep "\n" (v: ''
        if [[ ":''$${n}:" != *":${v}:"* ]]; then
          export ${n}="''$${n}:${v}"
        fi
      '') vs) suffixedVariables;
  in
  concatStringsSep "\n" exportVariables + "\n" +
  (concatMapStringsSep "\n" (p: ''
    if [[ " $NIX_PROFILES " != *" ${p} "* ]]; then
      export NIX_PROFILES="${p} $NIX_PROFILES"
    fi
  '') (reverseList dynProfiles));

in

{

  options.environment.dynamic-profiles = mkOption {
    type = types.attrsOf (types.submodule ({ name, ...}: {
      options = {
        packages = mkOption {
          type = types.listOf types.path;
          default = [];
        };
        extraSetup = mkOption {
          type = types.lines;
          default = "";
          example = "export ABC=def";
        };
        extraPostBuild = mkOption {
          type = types.lines;
          default = "";
          example = "ln -s some-file $out/some-dir/";
        };
        loadFile = mkOption {
          type = types.lines;
          default = "/etc/${etcDirName}/${name}/${loadFile}";
          internal = true;
          readOnly = true;
        };
      };
    }));
    description = ''
      Named dynamic profiles to be bound under /etc/${etcDirName}/<profile-name>.

      You can load it by including:

      . /etc/${etcDirName}/<profile-name>/${loadFile}
    '';
    default = {};
  };

  config = {
    environment.etc = mapAttrs' (n: pc: {
      name = "${etcDirName}/${n}";
      value.source = immutableProfile n pc;
    }) config.environment.dynamic-profiles;
  };
}
