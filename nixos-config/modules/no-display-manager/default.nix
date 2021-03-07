{ config, pkgs, lib, ... }:

let

  xlib = config.services.noDisplayManager.lib;
  inherit (lib) types;

  wmOptions = { name, ... }: {
    options = {

      launcherScript = lib.mkOption {
        type = types.path;
        description = "Path to DBus session launcher script.";
        example = xlib.withDbus { launcherScript = xlib.runStartx {}; };
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

    services.noDisplayManager.windowManager = lib.mkOption {
      type = types.attrsOf (types.submodule wmOptions);
      description = "Window manager launchers to define for all users. Accessible as TTY aliases.";
      default = {};
      example = {};
    };

    users.users = lib.mkOption {
      options = [{
        noDisplayManager.windowManager = lib.mkOption {
          type = types.attrsOf (types.submodule wmOptions);
          description = "Window manager launchers to define for this user. Accessible as TTY aliases.";
          default = {};
          example = {};
        };
      }];
    };

  };

  config = let

    usersWithWMs = lib.attrValues (lib.filterAttrs (n: u: u.noDisplayManager.windowManager != {}) config.users.users);

    dynamicProfileName = user: wmName:
      "nodm-${if user == null then "global" else "user-${user}"}-${wmName}";

    mkDynamicProfile = user: wmName: wm:
      let
        dpName = dynamicProfileName user wmName;
        loadProfileAndExec = pkgs.writeShellScript "start-wm" ''
          . ${config.environment.dynamic-profiles."${dpName}".loadFile}
          exec ${wm.launcherScript}
        '';
      in lib.nameValuePair dpName {
        inherit (wm) packages environment;
        extraPostBuild = "ln -s ${loadProfileAndExec} $out/start-wm";
      };

  in {

    environment.dynamic-profiles = (lib.mapAttrs' (
      mkDynamicProfile null
    ) config.services.noDisplayManager.windowManager)
    //
    (lib.foldl' (a: b: a // b) {} (map (u: lib.mapAttrs' (
      mkDynamicProfile u.name
    ) u.noDisplayManager.windowManager) usersWithWMs));

    environment.extraInit = let

      mkAlias = user: wmName: wm: ''
        alias ${wmName}='clear && exec ${builtins.dirOf config.environment.dynamic-profiles."${dynamicProfileName user wmName}".loadFile}/start-wm'
      '';

    in lib.mkIf ((lib.length usersWithWMs > 0) || config.services.noDisplayManager.windowManager != {}) ''
      # Only set noDisplayManager aliases in real console:
      if [ -n "$PS1" ]; then
        case "$(tty)" in /dev/tty[0-9]*)

          ${lib.concatStringsSep "\n" (lib.mapAttrsToList (
            mkAlias null
          ) config.services.noDisplayManager.windowManager)}

          ${lib.concatMapStringsSep "\n" (user: ''
            if [ "$USER" = ${lib.escapeShellArg user.name} ] ; then
              ${lib.concatStringsSep "\n" (lib.mapAttrsToList (
                mkAlias user.name
              ) user.noDisplayManager.windowManager)}
            fi
          '') usersWithWMs}
          ;;
        esac
      fi
    '';

  };

}
