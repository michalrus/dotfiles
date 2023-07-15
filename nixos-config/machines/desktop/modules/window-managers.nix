{ config, pkgs, lib, ... }:

let

  xlib = config.services.noDisplayManager.lib;

  i3wm = { hiDPI ? false, i3ExtraConf ? "", xresources ? "" }: {
    launcherScript = xlib.withDbus {
      journaldIdentifier = "i3";
      launcherScript = xlib.xorg.runStartx {
        pureXauthority = false; # for `emacs --daemon`
        extraXserverArgs = [ "-dpi" (if hiDPI then "192" else "96") ];
        loadXresources = false;
        xorgConf = xlib.xorg.mkXorgConf {
          hiDPIPointer = hiDPI;
          naturalScrollingTouchpad = true;
          naturalScrollingMouse = true;
          disablePointerWhileTyping = true;
          xkbLayout = "pl";
          xkbOptions = "compose:caps,numpad:microsoft";
        };
        windowManager = pkgs.writeShellScript "i3wm" ''
          ${if xresources != "" then ''
            ${pkgs.xorg.xrdb}/bin/xrdb <<< ${lib.escapeShellArg xresources}
          '' else ""}
          exec i3
        '';
      };
    };

    defaultArguments = {
      i3 = [ "-c" (import ./dotfiles/i3.conf.nix { inherit pkgs; extraConf = i3ExtraConf; }) ];
    };

    environment = {
      TERMINAL = "termite";
      _JAVA_AWT_WM_NONREPARENTING = "1";
      XMODIFIERS = "@im=none";  # For ~/.XCompose to…
      GTK_IM_MODULE = "xim";    #        … work in Gtk apps
      MOZ_USE_XINPUT2 = "1";    # For true Firefox smooth scrolling with touchpad.

    } // (if hiDPI then {
      XCURSOR_SIZE = "48";      # HiDPI mouse cursor.
    } else {});

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

  someXresources = ''
    #define S_base03        #002b36
    #define S_base02        #073642
    #define S_base01        #586e75
    #define S_base00        #657b83
    #define S_base0         #839496
    #define S_base1         #93a1a1
    #define S_base2         #eee8d5
    #define S_base3         #fdf6e3
    #define S_yellow        #b58900
    #define S_orange        #cb4b16
    #define S_red           #dc322f
    #define S_magenta       #d33682
    #define S_violet        #6c71c4
    #define S_blue          #268bd2
    #define S_cyan          #2aa198
    #define S_green         #859900
    ! Solarized dark
    Emacs.background:  S_base03
    Emacs.foreground:  S_base1
    Emacs.fadeColor:   S_base03
    Emacs.cursorColor: S_base1
    Emacs.pointerColor: S_base1
    Emacs.pointerColorBackground: S_base01
    Emacs.pointerColorForeground: S_base1
    Emacs.menuBar:            off
    Emacs.toolBar:            off
    Emacs.verticalScrollBars: off
    Emacs.font:               Iosevka:style=Regular:size=32
    Emacs.font-1:             unifont:fontformat=truetype:size=32:antialias=true
    Emacs.font-2:             FontAwesome:style=Regular:size=32

    Xcursor.theme: Adwaita
    Xcursor.size: 48
    XTerm*metaSendsEscape: true
    XTerm*utf8: 1
    XTerm*saveLines: 0
    XTerm*cursorBlink: true
    XTerm*selectToClipboard: true
    XTerm*charClass: 33:48,36-47:48,58-59:48,61:48,63-64:48,95:48,126:48
    XTerm*faceName: Iosevka:style=Regular:size=12
    XTerm*visualbell: true
    XTerm*bellIsUrgent: true
    XTerm*fullscreen: never
    XTerm*borderWidth: 0

    XTerm*translations: #override \
      Shift <Key>Insert:    insert-selection(SELECT) \n\
      Ctrl <Key>Insert:     copy-selection(SELECT)

    XTerm*background:  S_base03
    XTerm*foreground:  S_base1
    XTerm*color0: S_base02
    XTerm*color8: S_base02
    XTerm*color1: S_red
    XTerm*color9: S_orange
    XTerm*color2: S_green
    XTerm*color10: S_green
    XTerm*color3: S_yellow
    XTerm*color11: S_yellow
    XTerm*color4: S_blue
    XTerm*color12: S_blue
    XTerm*color5: S_magenta
    XTerm*color13: S_magenta
    XTerm*color6: S_cyan
    XTerm*color14: S_cyan
    XTerm*color7: S_base1
    XTerm*color15: S_base3
  '';

in {

  hardware.opengl.enable   = lib.mkDefault true;
  fonts.enableDefaultFonts = lib.mkDefault true;
  programs.dconf.enable    = lib.mkDefault true;

  security.pam.services.i3lock = {};

  #———————————————————————— ‘production’: i3 ————————————————————————

  services.noDisplayManager.windowManager.i3 = i3wm {
    hiDPI = true;
    xresources = someXresources;
  };

  services.noDisplayManager.windowManager.i3-lowdpi = i3wm {
    hiDPI = false;
    xresources = someXresources;
  };

  users.users.m.noDisplayManager.windowManager.i3 = i3wm {
    hiDPI = true;
    xresources = someXresources;
    i3ExtraConf = ''
      assign [class="^Firefox$" instance="^Navigator$"] 2

      assign [class="^mpv$"] 2

      assign [class="^[Qq]emu-"] 4
      assign [class="^VirtualBox$"] 4

      assign [class="^(XTerm|Termite)$" instance="^initial$"] 1
      assign [class="^Emacs$" title="^org$"] 1

      assign [class="^qjackctl$"] 4
      for_window [class="^qjackctl$"] [title="^JACK Audio Connection Kit"] floating enable

      assign [class="^(XTerm|Termite)$" instance="^mtr$"] 8

      assign [class="^Gnucash$"] 10
      assign [class="^nofatty$"] 10
      assign [class="^Transmission-gtk$"] 10
    '';
  };

  users.users.mw.noDisplayManager.windowManager.i3 = i3wm {
    hiDPI = true;
    xresources = someXresources;
    i3ExtraConf = ''
      assign [class="^Firefox$" instance="^Navigator$"] 2

      assign [class="^mpv$"] 2

      assign [class="^[Qq]emu-"] 4
      assign [class="^VirtualBox$"] 4

      assign [class="^(XTerm|Termite)$" instance="^initial$"] 1
      assign [class="^Emacs$" title="^org$"] 1

      assign [class="^Chromium-browser$"] 2

      assign [class="^VSCodium$"] 3

      assign [class="^(XTerm|Termite)$" instance="^code$"] 4
      assign [class="^Emacs$" title="^code$"] 4

      assign [class="^jetbrains-idea-ce$"] 5
      # IntelliJ’s splash screen:
      assign [class="^java-lang-Thread$"] 5

      assign [class="^zoom-us$"] 5
      assign [class="^zoom$"] 5
    '';
  };

  #———————————————————————— experiments: xterm ———————————————————————

  services.noDisplayManager.windowManager.xterm = {
    launcherScript = xlib.withDbus {
      launcherScript = xlib.xorg.runStartx {
        extraXserverArgs = [ "-dpi" "192" ];
        windowManager = "xterm";
      };
    };
    environment.MOZ_USE_XINPUT2 = "1";
    environment.EMACS_PROFILE = "black";
    packages = xlib.xorg.essentialPackages ++ (with pkgs; [ xterm ]);
  };

  #———————————————————————— experiments: sway ———————————————————————

  security.pam.services.swaylock = {};

  services.noDisplayManager.windowManager.sway = {
    launcherScript = xlib.withDbus {
      journaldIdentifier = "sway";
      launcherScript = pkgs.writeShellScript "sway" ''
        exec sway
      '';
    };
    packages = with pkgs; [
      sway swayidle swaylock xwayland wofi
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

}
