# Authored by Michael Fellinger <https://github.com/manveru>:
#
# XXX: Requires `systemctl --user start gnome-keyring.service`.
# XXX: And also a `login` keyring to be created within it.

{
  lib,
  stdenv,
  fetchurl,
  makeShellWrapper,
  binutils,
  zstd,
  dpkg,
  # Runtime dependencies
  alsa-lib,
  at-spi2-atk,
  at-spi2-core,
  atk,
  cairo,
  cups,
  dbus,
  expat,
  fontconfig,
  freetype,
  gdk-pixbuf,
  glib,
  gtk3,
  libGL,
  libappindicator-gtk3,
  libdrm,
  libgbm,
  libnotify,
  libpulseaudio,
  libsecret,
  libxcb,
  libxkbcommon,
  libxslt,
  nspr,
  nss,
  pango,
  pipewire,
  systemdLibs,
  udev,
  wayland,
  xdg-utils,
  xorg,
}:
stdenv.mkDerivation rec {
  pname = "jumpcloud-password-manager";
  version = "3.3.34";

  src = fetchurl {
    url = "https://cdn.pwm.jumpcloud.com/DA/release/x64/JumpCloud-Password-Manager-${version}.deb";
    hash = "sha256-nTCvtrorLy2056zBkpcQXpJEGH5uoKkWcyTcmT0SJRU=";
  };

  nativeBuildInputs = [
    binutils
    makeShellWrapper
    zstd
    dpkg
  ];

  buildInputs = [
    alsa-lib
    at-spi2-atk
    at-spi2-core
    atk
    cairo
    cups
    dbus
    expat
    fontconfig
    freetype
    gdk-pixbuf
    glib
    gtk3
    libGL
    libappindicator-gtk3
    libdrm
    libgbm
    libnotify
    libpulseaudio
    libsecret
    libxcb
    libxkbcommon
    libxslt
    nspr
    nss
    pango
    pipewire
    stdenv.cc.cc
    systemdLibs
    udev
    wayland
    xorg.libX11
    xorg.libXScrnSaver
    xorg.libXcomposite
    xorg.libXcursor
    xorg.libXdamage
    xorg.libXext
    xorg.libXfixes
    xorg.libXi
    xorg.libXrandr
    xorg.libXrender
    xorg.libXtst
    xorg.libxkbfile
    xorg.libxshmfence
  ];

  dontUnpack = true;
  dontPatchELF = true;

  installPhase =
    let
      rpath = lib.makeLibraryPath [
        alsa-lib
        at-spi2-atk
        at-spi2-core
        atk
        cairo
        cups
        dbus
        expat
        fontconfig
        freetype
        gdk-pixbuf
        glib
        gtk3
        libGL
        libappindicator-gtk3
        libdrm
        libgbm
        libnotify
        libpulseaudio
        libsecret
        libxcb
        libxkbcommon
        nspr
        nss
        pango
        pipewire
        stdenv.cc.cc
        systemdLibs
        udev
        wayland
        xorg.libX11
        xorg.libXScrnSaver
        xorg.libXcomposite
        xorg.libXcursor
        xorg.libXdamage
        xorg.libXext
        xorg.libXfixes
        xorg.libXi
        xorg.libXrandr
        xorg.libXrender
        xorg.libXtst
        xorg.libxkbfile
        xorg.libxshmfence
      ] + ":${lib.getLib stdenv.cc.cc}/lib64";
    in
    ''
      runHook preInstall

      # dpkg --fsys-tarfile avoids setuid issues with chrome-sandbox
      dpkg --fsys-tarfile $src | tar --extract
      rm -rf usr/share/lintian

      mkdir -p $out
      mv usr/* $out

      # Manual patchelf for all binaries and libraries
      for file in $(find $out -type f \( -perm /0111 -o -name \*.so\* \) ); do
        patchelf --set-interpreter "$(cat $NIX_CC/nix-support/dynamic-linker)" "$file" || true
        patchelf --set-rpath ${rpath}:$out/lib/jcpwm $file || true
      done

      # Create shell wrapper with Wayland support
      rm -f $out/bin/jcpwm
      makeShellWrapper $out/lib/jcpwm/jcpwm $out/bin/jcpwm \
        --prefix XDG_DATA_DIRS : "$GSETTINGS_SCHEMAS_PATH" \
        --suffix PATH : ${lib.makeBinPath [ xdg-utils ]} \
        --prefix LD_LIBRARY_PATH : ${lib.makeLibraryPath [ udev ]} \
        --add-flags "\''${NIXOS_OZONE_WL:+\''${WAYLAND_DISPLAY:+--ozone-platform-hint=auto --enable-features=WaylandWindowDecorations --enable-wayland-ime=true}}"

      runHook postInstall
    '';

  meta = {
    description = "JumpCloud Password Manager Desktop App";
    homepage = "https://jumpcloud.com";
    # license = lib.licenses.unfree;
    platforms = ["x86_64-linux"];
    maintainers = [];
  };
}
