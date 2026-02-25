{
  flake,
  config,
  lib,
  pkgs,
  ...
}: let
  inherit (config.networking) hostName;
in {
  programs.hyprland.enable = true;

  programs.hyprlock.enable = true;

  home-manager.sharedModules = [
    ({config, ...}: {
      home = {
        file = {
          ".config/hypr/hyprland.conf".text = ''
            source = ${./hyprland-monitor-lenovo-x1.conf}
            source = ${./hyprland.conf}
            source = ${
              if config.home.username == "mw" || config.home.username == "m"
              then ./hyprland-input-michalrus.conf
              else ./hyprland-input-generic.conf
            }
            ${
              if config.home.username == "m"
              then ''
                source = ${./hyprland-redshift.conf}
              ''
              else ""
            }
            #source = ${./hyprland-xwayland-hidpi.conf}
          '';

          ".config/fuzzel/fuzzel.ini".source = ./fuzzel.ini;

          ".config/hypr/hyprlock.conf".text =
            lib.replaceStrings ["screenshot"] ["${pkgs.hyprland}/share/hypr/wall0.png"]
            (builtins.readFile ./hyprlock.conf);

          ".config/swaync/style.css".source = ./swaync-style.css;

          ".config/hypr/hypridle.conf".text = ''
            ${builtins.readFile ./hypridle.conf}
            ${
              if hostName != "monstrum"
              then builtins.readFile ./hypridle-laptop.conf
              else builtins.readFile ./hypridle-desktop.conf
            }
          '';
        };

        pointerCursor = {
          gtk.enable = true;
          package = pkgs.vanilla-dmz;
          name = "Vanilla-DMZ";
        };

        sessionVariables = {
          HYPRCURSOR_THEME = config.home.pointerCursor.name;
          HYPRCURSOR_SIZE = 24;
          XCURSOR_SIZE = 24;
        };

        packages = [
          flake.packages.${pkgs.stdenv.hostPlatform.system}.hyprland-screenshot
          flake.packages.${pkgs.stdenv.hostPlatform.system}.wayland-logout
          flake.packages.${pkgs.stdenv.hostPlatform.system}.wayland-unicode-input
          flake.packages.${pkgs.stdenv.hostPlatform.system}.wayland-emoji-input
          flake.packages.${pkgs.stdenv.hostPlatform.system}.wayland-random-input
          pkgs.swaynotificationcenter
          pkgs.hypridle
          pkgs.wtype
          pkgs.grim
          pkgs.brightnessctl
          (pkgs.writeShellApplication {
            # for redshift:
            name = "wlsunset";
            runtimeInputs = [pkgs.wlsunset];
            text = ''
              latitude=51.26
              longitude=16.96
              kelvin_night=3700
              kelvin_day=6500
              exec wlsunset -l "$latitude" -L "$longitude" -t "$kelvin_night" -T "$kelvin_day"
            '';
          })
          pkgs.hicolor-icon-theme # Papirus fallback
          pkgs.wf-recorder
          pkgs.slurp
        ];
      };

      gtk = {
        enable = true;
        cursorTheme = {
          package = pkgs.vanilla-dmz;
          name = "Vanilla-DMZ";
          size = 24;
        };
        font = {
          name = "Noto Sans";
          size = 11;
        };
        iconTheme = {
          package = pkgs.papirus-icon-theme;
          name = "Papirus"; # "Papirus-Dark"
        };
        theme = {
          package = pkgs.kdePackages.breeze-gtk;
          name = "Breeze"; # "Breeze-Dark"
        };
      };
      qt.style.name = "breeze";

      programs = {
        fuzzel.enable = true;
        waybar = {
          enable = true;
          style = builtins.readFile ./waybar-style.css;
          settings = builtins.fromJSON (builtins.readFile ./waybar-config.json);
        };
      };
    })
  ];

  # Only set aliases in real console:
  environment.extraInit = lib.mkAfter ''
    if [ -n "$PS1" ]; then
      case "$(tty)" in /dev/tty[0-9]*)
        echo >&2
        echo >&2 -e "Run '\e[1mHyprland\e[0m' or '\e[1mhl\e[0m' to start a graphical environment."

        alias ${lib.escapeShellArg "Hyprland=clear && printf '\\e[3J' && exec Hyprland"}
        alias ${lib.escapeShellArg "hl=clear && printf '\\e[3J' && exec Hyprland"}
      ;;
      esac
    fi
  '';

  security.sudo = {
    enable = true;
    extraConfig = ''
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/loginctl lock-sessions
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/chvt 1
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/chvt 2
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/chvt 3
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/chvt 4
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/chvt 5
      %users ALL = (root) NOPASSWD: /run/current-system/sw/bin/chvt 6
    '';
  };
}
