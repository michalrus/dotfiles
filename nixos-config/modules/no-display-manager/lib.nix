{ config, pkgs, lib, ... }:

{

  options = {
    services.noDisplayManager.lib = lib.mkOption {
      type = lib.types.attrs;
      readOnly = true;
      description = ''
        A few reusable utils to help with window manager definitions.

        Example: how to call those functions in your modules:

        ```
        let
          xlib = config.services.noDisplayManager.lib;
        in
          xlib.withDbus { … }
        ```
      '';

      default = {

        ##
        #  Run the `launcherScript` in a DBus session, logging its stderr/stdout to
        #  journald under `journaldIdentifier`.
        #
        withDbus = {
          journaldIdentifier ? "window-manager",
          launcherScript
        }:
          pkgs.writeShellScript "with-dbus" ''
            exec ${pkgs.dbus}/bin/dbus-launch --exit-with-session \
              ${config.systemd.package}/bin/systemd-cat -t ${lib.escapeShellArg journaldIdentifier} \
              ${lib.escapeShellArg launcherScript}
            '';

        ##
        #  X11-specific functions:
        #
        xorg = rec {

          ##
          #  Probably a good idea to add them to your launcher’s
          #  `.packages`, if running X11.
          #
          essentialPackages = with pkgs.xorg; [
            xauth xdpyinfo xev xhost xinput xmodmap xprop xrandr xrdb xset
          ];

          ##
          #  Generates a script that will properly start the X server.
          #
          runStartx = {
            # Script/ELF to start your chosen WM. Has to be absolute.
            windowManager ? "${pkgs.xterm}/bin/xterm",

            # Path to `xorg.conf`.
            xorgConf ? mkXorgConf {},

            # Extra arguments to pass to xserver, e.g. `[ "-dpi" "192" ]`.
            extraXserverArgs ? [],

            # Whether to load `~/.Xresources` into Xserver.
            loadXresources ? true,

            # If true, keep `.Xauthority` in `/run/user/`. If false, in `~/.Xauthority`.
            # Note that e.g. Emacs daemon has problems with that, when trying
            # to use a single one in 2 concurrent X sessions of the same
            # user. Not sure why the *daemon* requires access to .Xauthority,
            # not just the graphical client…
            pureXauthority ? true
          }:
            assert (lib.assertMsg (lib.hasPrefix "/" windowManager) "window manager path needs to be absolute.");
            pkgs.writeShellScript "run-startx" ''
              exec ${pureStartx { inherit pureXauthority; }} \
                ${if loadXresources then pkgs.writeShellScript "load-xresources" ''
                  xrdb $HOME/.Xresources || true
                  exec ${lib.escapeShellArg windowManager}
                '' else lib.escapeShellArg windowManager} \
                -- \
                -config ${lib.escapeShellArg xorgConf} \
                -xkbdir ${pkgs.xkeyboard_config}/etc/X11/xkb \
                -logfile /dev/null -logverbose 3 \
                -nolisten tcp -novtswitch ${lib.escapeShellArgs extraXserverArgs}
            '';

          ##
          #  A better, purer `${pkgs.xorg.xinit}/bin/startx`:
          #
          pureStartx = { pureXauthority }: pkgs.runCommand "pure-startx" { preferLocalBuild = true; } ''
            cp ${pkgs.xorg.xinit}/bin/startx $out
            chmod 755 $out

            # Don’t leave a hanging `bin/sh`, instead become `xinit`, and don’t require `xinit` on `PATH`:
            sed -r 's#^xinit #exec ${pkgs.xorg.xinit}/bin/xinit #g' -i $out

            # Don’t use config files in HOME:
            sed -r 's/^(userclientrc)=.*/unset \1/g' -i $out
            sed -r 's/^(userserverrc)=.*/unset \1/g' -i $out

            # Log to stdout/stderr:
            sed -r 's#\$HOME/\.xorg\.log#/dev/null#g' -i $out

            # Keep serverauth in `/run/user/`, separate for each xserver:
            sed -r 's#HOME/\.serverauth#XDG_RUNTIME_DIR/.Xserverauth#g' -i $out

            # Don’t unset `DBUS_SESSION_BUS_ADDRESS`:
            sed -r '/^unset DBUS_/d' -i $out

            ${if !pureXauthority then "" else ''
              # Keep `.Xauthority` in `/run/user/`, separate for each xserver:
              sed -r '/^enable_xauth=/a export XAUTHORITY=$XDG_RUNTIME_DIR/.Xauthority.$$' -i $out
            ''}
          '';

          ##
          #  Opinionated `xorg.conf` that works for @michalrus:
          #
          mkXorgConf = {

            # If true, make mouse/touchpad 2× faster:
            hiDPIPointer ? false,

            # Enable natural scrolling on all pointer devices:
            leftHandedPointer ? false,
            middleEmulation ? true,
            naturalScrollingTouchpad ? true,
            naturalScrollingMouse ? true,
            horizontalScrolling ? true,
            tapping ? true,
            tappingDragLock ? true,
            disablePointerWhileTyping ? false,

            # Keyboard:
            xkbLayout  ? "us",
            xkbModel   ? "pc104",
            xkbOptions ? "terminate:ctrl_alt_bksp",
            xkbVariant ? "", # e.g. dvorak, colemak etc.

            # E.g. extra video drivers (Nvidia?):
            extraModules ? [],
            # Some other fonts you want on FontPath:
            extraFonts ? [],

            # Additional config at the end of `xorg.conf`:
            extraConfig ? ""

          }: let
            xorgBool = v: if v then "on" else "off";
          in pkgs.writeText "no-dm-xorg.conf" ''

            ${xorgConfImportPaths { inherit extraModules extraFonts; }}
            ${xorgConfImportLibinput}

            # Keyboard options:
            Section "InputClass"
              Identifier "Keyboard catchall"
              MatchIsKeyboard "on"
              Option "XkbModel" "${xkbModel}"
              Option "XkbLayout" "${xkbLayout}"
              Option "XkbOptions" "${xkbOptions}"
              Option "XkbVariant" "${xkbVariant}"
            EndSection

            # Touchpad options:
            Section "InputClass"
              Identifier "libinputConfiguration"
              MatchIsTouchpad "on"
              Driver "libinput"
              Option "AccelProfile" "adaptive"

              ${if hiDPIPointer then ''
                Option "AccelSpeed" "0.2"
                Option "TransformationMatrix" "2 0 0 0 2 0 0 0 1"
              '' else ''
                Option "AccelSpeed" "0.1"
              ''}

              Option "LeftHanded" "${xorgBool leftHandedPointer}"
              Option "MiddleEmulation" "${xorgBool middleEmulation}"

              Option "NaturalScrolling" "${xorgBool naturalScrollingTouchpad}"
              Option "ScrollMethod" "twofinger"
              Option "HorizontalScrolling" "${xorgBool horizontalScrolling}"
              Option "SendEventsMode" "enabled"
              Option "Tapping" "${xorgBool tapping}"
              Option "TappingDragLock" "${xorgBool tappingDragLock}"
              Option "DisableWhileTyping" "${xorgBool disablePointerWhileTyping}"
            EndSection

            # Mouse options:
            Section "InputClass"
              Identifier "libinput pointer catchall"
              MatchIsPointer "on"
              MatchIsTouchpad "off"
              Driver "libinput"
              MatchDevicePath "/dev/input/event*"
              Option "NaturalScrolling" "${xorgBool naturalScrollingMouse}"

              ${if hiDPIPointer then ''
                Option "AccelSpeed" "0.2"
                Option "TransformationMatrix" "2 0 0 0 2 0 0 0 1"
              '' else ''
                Option "AccelSpeed" "0.1"
              ''}
            EndSection

            ${extraConfig}

          '';

          ##
          #  Include this in your `xorg.conf`, if you use libinput.
          #
          xorgConfImportLibinput = ''
            # Prevent GC of ${pkgs.xorg.xf86inputlibinput}
            ${builtins.readFile "${pkgs.xorg.xf86inputlibinput}/share/X11/xorg.conf.d/40-libinput.conf"}
          '';

          ##
          #  You most likely need to include these imports in your custom `xorg.conf`.
          #
          #  Adapted from <https://github.com/NixOS/nixpkgs/blob/07bc7b971dc34b97a27ca9675a19f525a7d3616e/nixos/modules/services/x11/xserver.nix#L115-L142>.
          #
          xorgConfImportPaths = {
            # E.g. extra video drivers (Nvidia?):
            extraModules ? [],
            # Some other fonts you want on FontPath:
            extraFonts ? [],

            # These you probably shouldn’t touch, but configurable nonetheless:
            internalAllModules ?
              (with pkgs.xorg; [
                xf86videoati
                xf86videocirrus
                xf86videovesa
                xf86videovmware
                xorgserver
                xf86inputevdev
                xf86inputlibinput
              ]) ++ extraModules,

            # These you probably shouldn’t touch, but configurable nonetheless:
            internalAllFonts ?
              config.fonts.fonts ++ (with pkgs.xorg; [ fontadobe100dpi fontadobe75dpi ]) ++ extraFonts
          }: let
            sectionFiles = pkgs.runCommand "xserver-paths.conf" { preferLocalBuild = true; } ''
              echo 'Section "Files"' >> $out
              for i in ${lib.escapeShellArgs internalAllFonts}; do
                if test "''${i:0:''${#NIX_STORE}}" == "$NIX_STORE"; then
                  for j in $(find $i -name fonts.dir); do
                    echo "  FontPath \"$(dirname $j)\"" >> $out
                  done
                fi
              done
              for i in $(find ${lib.escapeShellArgs internalAllModules} -type d); do
                if test $(echo $i/*.so* | wc -w) -ne 0; then
                  echo "  ModulePath \"$i\"" >> $out
                fi
              done
              echo 'EndSection' >> $out
            '';
          in ''
            # Prevent GC of ${sectionFiles}
            ${builtins.readFile sectionFiles}
          '';

        };

      };
    };
  };

}
