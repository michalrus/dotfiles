{
  stdenv,
  fetchzip,
  autoPatchelfHook,
  gcc,
  libz,
  freetype,
  fontconfig,
  libGL,
  libxcb,
  libXrender,
  writeShellApplication,
}: let
  # This is the same exact hash as on the installation CD added to AmScope MU1403:
  #
  # src-2018 = fetchzip {
  #   url = "https://storage.googleapis.com/software-download-d79bb.appspot.com/software/AmLite/Linux/20180326/AmScopeAmLite_20180326.tar";
  #   hash = "sha256-18Yb27zmd+ImLkZSXQ5mrkIFltL/w9py27tWd9memDM=";
  #   stripRoot = false;
  # };
  #
  unsafe = stdenv.mkDerivation rec {
    pname = "AmLite";
    version = "20232603";
    src = fetchzip {
      url = "https://storage.googleapis.com/software-download-d79bb.appspot.com/software/AmLite/Linux/${version}/AmScopeAmLite.x64.tar.bz2";
      hash = "sha256-LAhLrIxxlugpYIkHyv0MY6LPW281zO+QuJI5yZqh3jM=";
    };
    nativeBuildInputs = [autoPatchelfHook];
    buildInputs = [
      gcc.cc
      libz
      freetype
      fontconfig
      libxcb
      libXrender
      libGL
    ];
    dontBuild = true;
    installPhase = ''
      mkdir -p $out/libexec
      sed -n -e '1,/^exit 0$/!p' AmScopeAmLite.x64.sh | tar -C $out/libexec -xzvf -

      mkdir -p $out/bin
      ln -s $out/libexec/AmLite $out/bin/AmLite
      rm $out/libexec/uninstall.sh

      mkdir -p $out/lib/udev/rules.d
      mv $out/libexec/*.rules $out/lib/udev/rules.d/

      mkdir -p $out/share/icons/hicolor/128x128/apps/
      mv $out/libexec/AmLite.png $out/share/icons/hicolor/128x128/apps/

      mkdir -p $out/share/applications
      mv $out/libexec/AmLite.desktop $out/share/applications
      sed -r 's,^Exec=.*,Exec=AmLite,g ; s,^Icon=.*,Icon=AmLite,g' -i $out/share/applications/AmLite.desktop
    '';
  };

  bwrap = writeShellApplication {
    name = "AmLite";
    text = ''
      mkdir -p "$HOME"/Pictures/AmLite
      exec bwrap \
        --unshare-all --new-session --die-with-parent \
        --clearenv \
        --proc /proc \
        --dev /dev \
        --dev-bind /dev/bus/usb /dev/bus/usb \
        --tmpfs /tmp \
        --tmpfs /run/user/"$UID" \
        --setenv XDG_RUNTIME_DIR /run/user/"$UID" \
        --ro-bind /nix/store /nix/store \
        --ro-bind /etc/fonts /etc/fonts \
        --ro-bind /tmp/.X11-unix /tmp/.X11-unix \
        --setenv DISPLAY "$DISPLAY" \
        --ro-bind "''${XAUTHORITY:-$HOME/.Xauthority}" /tmp/.Xauthority \
        --setenv XAUTHORITY /tmp/.Xauthority \
        --bind "$HOME"/Pictures/AmLite /home/"$USER" \
        --setenv HOME /home/"$USER" \
        --setenv LIBUSB_DEBUG 4 \
        --tmpfs /dev/shm \
        --setenv QT_X11_NO_MITSHM 1 \
        ${unsafe}/bin/AmLite
    '';
  };
in
  bwrap
