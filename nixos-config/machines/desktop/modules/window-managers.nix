{ config, pkgs, lib, ... }:

let

  xlib = config.services.noDisplayManager.lib;

in {

  services.noDisplayManager.windowManager.xterm = {
    launcherScript = xlib.withDbus {
      launcherScript = xlib.xorg.runStartx {
        extraXserverArgs = [ "-dpi" "192" ];
        #windowManager = "${pkgs.xterm}/bin/xterm";
        windowManager = pkgs.writeShellScript "xterm-xrdb" ''
          ${ builtins.readFile ../../../../dotfiles/i3/.config/autostart/dotfiles.d/15xresources-merge }
          exec xterm
        '';
      };
    };
    environment.MOZ_USE_XINPUT2 = "1";
    packages = xlib.xorg.essentialPackages ++ (with pkgs; [ xterm ]);
  };

  hardware.opengl.enable   = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  security.pam.services.swaylock = {};

  services.noDisplayManager.windowManager.sway = {
    launcherScript = xlib.withDbus {
      journaldIdentifier = "sway";
      launcherScript = pkgs.writeShellScript "sway" ''
        exec sway
      '';
    };
    packages = with pkgs; [
      sway swayidle swaylock xwayland nixos-unstable.wofi
      termite firefox-wayland

      # For Xwayland’s DPI setting:
      xorg.xeyes xorg.xdpyinfo xorg.xrandr xorg.xrdb
    ];
    environment = {
      QT_QPA_PLATFORM = "wayland-egl";
      QT_WAYLAND_FORCE_DPI = "physical";
      QT_WAYLAND_DISABLE_WINDOWDECORATION = "1";
      ECORE_EVAS_ENGINE = "wayland_egl";
      ELM_ENGINE = "wayland_egl";
      SDL_VIDEODRIVER = "wayland";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      MOZ_ENABLE_WAYLAND = "1";
      SAL_USE_VCLPLUGIN = "gtk3";
    };
  };

  security.pam.services.i3lock = {};

  services.noDisplayManager.windowManager.i3 = {
    launcherScript = xlib.withDbus {
      journaldIdentifier = "i3";
      launcherScript = xlib.xorg.runStartx {
        pureXauthority = false; # for `emacs --daemon`
        extraXserverArgs = [ "-dpi" "192" ];
        loadXresources = false;
        xorgConf = xlib.xorg.mkXorgConf {
          hiDPIPointer = true;
          naturalScrollingTouchpad = true;
          naturalScrollingMouse = true;
          disablePointerWhileTyping = true;
          xkbLayout = "pl";
          xkbOptions = "compose:caps,numpad:microsoft";
        };

        # Optionally, merge configs, because `i3` doesn’t have an
        # `include` directive… <https://github.com/i3/i3/issues/1197>
        #
        # To use that, create a few ~/.config/i3/*.conf files.
        #
        # Note, that if you use `*.conf` instead of a single `config`,
        # this will effectively break `i3-msg reload`.
        windowManager = pkgs.writeShellScript "i3-merged-configs" ''
          configOpt=""
          if ls ~/.config/i3/*.conf 1>/dev/null 2>&1 ; then
            dir=$XDG_RUNTIME_DIR/i3
            mkdir -p $dir
            cnf=$(mktemp -p $dir config.XXXXXXX)
            find ~/.config/i3 -name '*.conf' -print0 | sort -z | xargs -r0 cat > $cnf
            configOpt=" -c $cnf "
          fi

          exec ${pkgs.i3}/bin/i3 $configOpt
        '';
      };
    };

    environment = {
      TERMINAL = "termite";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      XMODIFIERS = "@im=none";  # For ~/.XCompose to…
      GTK_IM_MODULE = "xim";    #        … work in Gtk apps
      MOZ_USE_XINPUT2 = "1";    # For true Firefox smooth scrolling with touchpad.
      XCURSOR_SIZE = "48";      # HiDPI mouse cursor.
    };

    packages = xlib.xorg.essentialPackages ++ (with pkgs; [

      # These packages will be visible from within `i3` session only.
      i3 i3lock i3status
      rofi michalrus.dmenu-rofi dunst
      compton
      autocutsel
      peek

      termite firefox

      arandr
      wmctrl xtitle
      xrandr-invert-colors
      unclutter xbanish
      xautolock

      xcape xdo xdotool
      xclip xsel
      xpad

      xterm
      xorg.xeyes
      xorg.xclock
      xorg.xbacklight

    ]);
  };

}
