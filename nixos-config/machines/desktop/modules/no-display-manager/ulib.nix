{ config, pkgs }:

rec {

  #
  # We need to be able to add (export) stuff to the user env before
  # running a particular graphical environment. This includes not only
  # trivial PATH, but also things like TERMINFO_DIRS, QT_PLUGIN_PATH,
  # INFOPATH etc. Let’s use `buildEnv` which does precisely that.
  #
  exportProfileWithPkgs = name: paths: exportDynamicProfiles [(
    pkgs.buildEnv {
      name = "dynamic-profile-${name}";
      inherit paths;
      inherit (config.environment) pathsToLink extraOutputsToInstall;
      inherit (config.system.path) ignoreCollisions postBuild;
    }
  )];

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
    exportVariables = mapAttrsToList (n: v: "export ${n}=\"\$${n}:${concatStringsSep ":" v}\"") suffixedVariables;
  in
  concatStringsSep "\n" exportVariables + "\n" + ''
    export NIX_PROFILES="${concatStringsSep " " (reverseList dynProfiles)} $NIX_PROFILES"
  '';

  do-startx = wm:
    assert (pkgs.lib.assertMsg (pkgs.lib.hasPrefix "/" wm) "window manager path needs to be absolute.");
    " ${better-startx} ${wm} -- -config ${xorgConf} -xkbdir ${pkgs.xkeyboard_config}/etc/X11/xkb "
    + " -logfile /dev/null -logverbose 3 -nolisten tcp -novtswitch ";

  #
  # It won’t leave a hanging /bin/sh, instead becoming `xinit`. Also,
  # we won’t add either `startx` or `xinit` to session’s PATH
  # unnecessarily.  Also, `serverauth.PID` will be kept in
  # /run/user/UID.
  #
  better-startx = pkgs.runCommand "startx" { preferLocalBuild = true; } ''
    cp ${pkgs.xorg.xinit}/bin/startx $out
    chmod 755 $out
    sed 's#^xinit #exec ${pkgs.xorg.xinit}/bin/xinit #g'    -i $out
    sed 's#HOME/.serverauth#XDG_RUNTIME_DIR/.Xserverauth#g' -i $out
  '';

  #
  # We need a non-global `xorg.conf`, similar to the one generated
  # globally by NixOS modules. This seems to a be a reasonable minimal
  # one.
  #
  # Remember to tweak your keyboard settings (`XkbOptions`) below!
  #
  # Adapted from <https://github.com/NixOS/nixpkgs/blob/07bc7b971dc34b97a27ca9675a19f525a7d3616e/nixos/modules/services/x11/xserver.nix#L115-L142>.
  #
  xorgConf = let
    modules = with pkgs.xorg; [
      xf86videoati
      xf86videocirrus
      xf86videovesa
      xf86videovmware
      xorgserver
      xf86inputevdev
      xf86inputlibinput
    ];
    fontsForXServer = config.fonts.fonts ++ (with pkgs.xorg; [ fontadobe100dpi fontadobe75dpi ]);
    sectionFiles = pkgs.runCommand "xserver.conf" { preferLocalBuild = true; } ''
      echo 'Section "Files"' >> $out
      for i in ${toString fontsForXServer}; do
        if test "''${i:0:''${#NIX_STORE}}" == "$NIX_STORE"; then
          for j in $(find $i -name fonts.dir); do
            echo "  FontPath \"$(dirname $j)\"" >> $out
          done
        fi
      done
      for i in $(find ${toString modules} -type d); do
        if test $(echo $i/*.so* | wc -w) -ne 0; then
          echo "  ModulePath \"$i\"" >> $out
        fi
      done
      echo 'EndSection' >> $out
    '';

  in pkgs.writeText "xorg.conf" ''
    ${builtins.readFile sectionFiles}

    # Automatically enable the libinput driver for all touchpads.
    Section "InputClass"
      Identifier "libinputConfiguration"
      MatchIsTouchpad "on"
      Driver "libinput"
      Option "AccelProfile" "adaptive"
      Option "AccelSpeed" "0.1"
      Option "LeftHanded" "off"
      Option "MiddleEmulation" "on"
      Option "NaturalScrolling" "on"
      Option "ScrollMethod" "twofinger"
      Option "HorizontalScrolling" "on"
      Option "SendEventsMode" "enabled"
      Option "Tapping" "on"
      Option "TappingDragLock" "on"
      Option "DisableWhileTyping" "off"
    EndSection

    Section "InputClass"
      Identifier "Keyboard catchall"
      MatchIsKeyboard "on"
      Option "XkbModel" "pc104"
      Option "XkbLayout" "pl"
      Option "XkbOptions" "compose:caps,numpad:microsoft"
      Option "XkbVariant" ""
    EndSection

    ${builtins.readFile "${pkgs.xorg.xf86inputlibinput}/share/X11/xorg.conf.d/40-libinput.conf"}
  '';

  ifTTY = code: ''
    if [ -n "$PS1" ]; then
      case "$(tty)" in /dev/tty[0-9]*)
        ${code}
        ;;
      esac
    fi
  '';

}
