{ flake, config, pkgs, lib, ... }:

let

  users = {
    m = {
      i3-new = mkI3 rec { hiDPI = true; extraConf = import ./window-managers/i3-m.conf.nix; };
      i3-low-new = mkI3 rec { hiDPI = false; extraConf = import ./window-managers/i3-m.conf.nix; };
    };
    mw = {
      i3-new = mkI3 rec { hiDPI = true; extraConf = import ./window-managers/i3-mw.conf.nix; };
    };
    md = {
      i3-new = mkI3 rec { hiDPI = true; };
    };
    guest = {
      i3-new = mkI3 rec { hiDPI = true; };
      i3-low-new = mkI3 rec { hiDPI = false; };
    };
  };

  mkI3 = { hiDPI ? false, extraConf ? "" }: (common { inherit hiDPI; }).override {
    windowManager = pkgs.writeShellScript "i3" ''
      export TERMINAL=termite
      export _JAVA_AWT_WM_NONREPARENTING=1
      export XMODIFIERS="@im=none"  # For ~/.XCompose to…
      export GTK_IM_MODULE=xim      #        … work in Gtk apps
      export MOZ_USE_XINPUT2=1      # For true Firefox smooth scrolling with touchpad.

      exec i3 -c ${pkgs.writeText "i3.conf" (
        import ./window-managers/i3.conf.nix
        + extraConf
      )}
    '';
    journaldIdentifier = "i3-new";
    loadXresources = pkgs.writeText "xresources" (import ./window-managers/xresources.nix { inherit hiDPI; });
    extraPackages = with pkgs; [
      peek
      termite

      i3 i3lock i3status
      rofi flake.packages.${pkgs.system}.dmenu-is-rofi
      dunst
      compton
      autocutsel
      arandr
      wmctrl xtitle
      xrandr-invert-colors
      unclutter xbanish
      xautolock
      xcape xdo xdotool
      xclip xsel
      xpad
      xorg.xeyes
      xorg.xclock
      xorg.xbacklight
    ];
  };

  common = { hiDPI }: pkgs.callPackage ../../../packages/x11-rootless rec {
    xserverConfig = {
      layout = "pl";
      xkbOptions = "compose:caps,numpad:microsoft";
      videoDrivers = [ "intel" /* unfree "nvidia" */ "modesetting" "fbdev" ];
      upscaleDefaultCursor = hiDPI;
      dpi = if hiDPI then 192 else 96;
      libinput = rec {
        touchpad = mouse;
        mouse = {
          naturalScrolling = true;
          accelSpeed = if hiDPI then "0.2" else "0.1";
          transformationMatrix = if hiDPI then "2 0 0 0 2 0 0 0 1" else null;
          disableWhileTyping = true;
        };
      };
    };
    pureXauthority = false;  # Doesn’t work with ‘emacs-daemon’
    windowManager = "${pkgs.xterm}/bin/xterm";
    inherit (config.environment) profileRelativeEnvVars;
  };

in

{
  environment.extraInit = ''
    # Only set aliases in real console:
    if [ -n "$PS1" ]; then
      case "$(tty)" in /dev/tty[0-9]*)
        ${lib.concatStringsSep "\n" (lib.mapAttrsToList (user: wms: ''
          if [ "$USER" = ${lib.escapeShellArg user} ] ; then
            ${lib.concatStringsSep "\n" (lib.mapAttrsToList (wm: exe: ''
              alias ${lib.escapeShellArg "${wm}=clear && printf '\\e[3J' && exec ${exe}"}
            '') wms)}
          fi
        '') users)}
      ;;
      esac
    fi
  '';
}
