{ config, pkgs, lib, ... }:

let

  xlib = config.services.noDisplayManager.lib;
  inherit (lib) types;

  launcherOptions = { name, ... }: {
    options = {

      launcherScript = lib.mkOption {
        type = types.path;
        description = "Path to DBus session launcher script.";
        example = xlib.withDbus { command = xlib.runStartx {}; };
      };

      packages = lib.mkOption {
        type = types.listOf types.package;
        description = "Packages accessible from within this window manager.";
        default = [];
      };

      environment = lib.mkOption {
        type = with types; attrsOf (nullOr (oneOf [ str path package ]));
        description = "Environment variables to set in this window manager session.";
        default = {};
        example = { MOZ_USE_XINPUT2 = "1"; };
      };

    };

    config = {
      environment.DESKTOP_SESSION = name;
    };
  };

in

{

  imports = [
    ./lib.nix
    ../dynamic-profiles.nix
  ];

  options = {

    services.noDisplayManager.launchers = lib.mkOption {
      type = types.attrsOf (types.submodule launcherOptions);
      description = "Window manager launchers to define for all users. Accessible as TTY aliases.";
      default = {};
      example = {};
    };

    users.users = lib.mkOption {
      options = [{
        noDisplayManager.launchers = lib.mkOption {
          type = types.attrsOf (types.submodule launcherOptions);
          description = "Window manager launchers to define for this user. Accessible as TTY aliases.";
          default = {};
          example = {};
        };
      }];
    };

  };

  config = let

    dynamicProfileName = launcherName: "nodm-global-${launcherName}";

  in {

    environment.dynamic-profiles = lib.mapAttrs' (launcherName: launcher:
      let
        dpName = dynamicProfileName launcherName;
        loadProfileAndExec = pkgs.writeShellScript "start-wm" ''
          . ${config.environment.dynamic-profiles."${dpName}".loadFile}
          exec ${launcher.launcherScript}
        '';
      in lib.nameValuePair dpName {
        inherit (launcher) packages environment;
        extraPostBuild = "ln -s ${loadProfileAndExec} $out/start-wm";
      }
    ) config.services.noDisplayManager.launchers;

    environment.extraInit = ''
      # Only set noDisplayManager aliases in real console:
      if [ -n "$PS1" ]; then
        case "$(tty)" in /dev/tty[0-9]*)

          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (launcherName: launcher: ''
            alias ${launcherName}='clear && exec ${builtins.dirOf config.environment.dynamic-profiles."${dynamicProfileName launcherName}".loadFile}/start-wm'
          '') config.services.noDisplayManager.launchers)}

          ;;
        esac
      fi
    '';

  };

}
