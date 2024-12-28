{ flake, config, lib, pkgs, ... }:

let inherit (config.networking) hostName; in

{
  programs.hyprland.enable = true;
  programs.hyprlock.enable = true;

  home-manager.sharedModules = [({ config, ... }: {
    home.file.".config/hypr/hyprland.conf".text = ''
      source = ${./hyprland-monitor-lenovo-x1.conf}
      source = ${./hyprland.conf}
      source = ${
        if config.home.username == "mw" || config.home.username == "m" then
          ./hyprland-input-michalrus.conf
        else
          ./hyprland-input-generic.conf
      }
      #source = ${./hyprland-xwayland-hidpi.conf}
    '';

    home.pointerCursor.gtk.enable = true;
    home.pointerCursor.package = pkgs.vanilla-dmz;
    home.pointerCursor.name = "Vanilla-DMZ";
    home.sessionVariables.HYPRCURSOR_THEME = config.home.pointerCursor.name;
    home.sessionVariables.HYPRCURSOR_SIZE = 24;
    home.sessionVariables.XCURSOR_SIZE = 24;

    programs.fuzzel.enable = true;
    home.file.".config/fuzzel/fuzzel.ini".source = ./fuzzel.ini;

    programs.waybar.enable = true;
    programs.waybar.style = builtins.readFile ./waybar-style.css;
    programs.waybar.settings = builtins.fromJSON (builtins.readFile ./waybar-config.json);

    home.file.".config/hypr/hyprlock.conf".text =
      lib.replaceStrings ["screenshot"] ["${pkgs.hyprland}/share/hypr/wall0.png"]
      (builtins.readFile ./hyprlock.conf);

    home.file.".config/swaync/style.css".source = ./swaync-style.css;

    home.file.".config/hypr/hypridle.conf".source = ./hypridle.conf;

    home.packages = [
      flake.packages.${pkgs.system}.hyprland-screenshot
      flake.packages.${pkgs.system}.wayland-logout
      (flake.packages.${pkgs.system}.wayland-unicode-input.override { onlyEmoji = false; })
      (flake.packages.${pkgs.system}.wayland-unicode-input.override { onlyEmoji = true;  })
      pkgs.swaynotificationcenter
      pkgs.hypridle
    ];
  })];

  # Only set aliases in real console:
  environment.extraInit = lib.mkAfter ''
    if [ -n "$PS1" ]; then
      case "$(tty)" in /dev/tty[0-9]*)
        echo >&2
        echo >&2 -e "Run '\e[1mHyprland\e[0m' or '\e[1mhl\e[0m' to start a graphical environment."

        alias ${lib.escapeShellArg "Hyprland=clear && printf '\\e[3J' && exec Hyprland"}
        alias ${lib.escapeShellArg       "hl=clear && printf '\\e[3J' && exec Hyprland"}
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
