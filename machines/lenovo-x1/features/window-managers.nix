{ flake, config, pkgs, lib, ... }:

let

  users = {
    m.i3 = mkI3 { extraConf = import ./window-managers/i3-m.conf.nix; };
    mw.i3 = mkI3 { extraConf = import ./window-managers/i3-mw.conf.nix; };
    md.i3 = mkI3 {};
    guest.i3 = mkI3 {};
    guest.i3-low = mkI3 { hiDPI = false; };  # projector
  };

  mkI3 = { hiDPI ? true, extraConf ? "" }: (common { inherit hiDPI; }).override {
    windowManager = pkgs.writeShellScript "i3" ''
      export TERMINAL=alacritty
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
      alacritty

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
    extraFonts = config.fonts.fonts;
    pureXauthority = false;  # Doesn’t work with ‘emacs-daemon’
    windowManager = "${pkgs.xterm}/bin/xterm";
    inherit (config.environment) profileRelativeEnvVars;
  };

in

{
  hardware.opengl.enable   = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  security.pam.services.i3lock = {};

  services.xserver = {
    enable = lib.mkForce false;  # no global X11 – see ‘flake.nixosModules.no-display-manager’
    layout = lib.mkDefault "pl";
    synaptics = {
      enable = lib.mkDefault true;
      twoFingerScroll = true;
      tapButtons = true;
      fingersMap = [1 3 2];
    };
  };

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
