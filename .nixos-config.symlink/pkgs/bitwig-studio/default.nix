super: self:

let

  libredirect = super.libredirect.overrideDerivation (oldAttrs: {
     patches = [ ./libredirect.patch ];
  });

in

super.stdenv.mkDerivation rec {
  p_name = "bitwig-studio";
  version = "1.3.9";

  name = "${p_name}-${version}";

  src = super.fetchurl {
    url = "https://downloads.bitwig.com/stable/${version}/${p_name}-${version}.deb";
    sha256 = "b174865d884c875d8e849215e6ab54c3e179df2eab52b2bdc2bf7a6ead3793cb";
  };

  buildInputs = with super; [ dpkg makeWrapper ];

  unpackCmd = "mkdir root ; dpkg-deb -x $curSrc root";

  dontBuild = true;
  dontPatchELF = true;
  dontStrip = true;

  libpath = super.lib.makeLibraryPath (with super; [
    alsaLib bzip2.out cairo freetype glib harfbuzz libbsd libjack2
    libpng pixman zlib
  ] ++ (with xorg; [
    libxcb xcbutil xcbutilwm libxkbfile
    libX11 libXau libXcursor libXdmcp libXext libXfixes libXrender
  ]));

  binpath = super.lib.makeBinPath (with super; [
    gnome2.zenity xdg_utils
  ]);

  installPhase = ''
    mkdir -p $out
    cp -r opt/bitwig-studio $out/libexec

    for f in bitwig-studio bin/BitwigPluginHost64 bin/BitwigStudioEngine bin/bitwig-vamphost ; do
      patchelf \
        --set-interpreter $(cat ${super.stdenv.cc}/nix-support/dynamic-linker) \
        $out/libexec/$f
    done

    # Use NixOS versions of these libs instead of the bundled ones.
    (
      cd $out/libexec/lib/bitwig-studio
      rm libbz2.so* libxkbfile.so* libXcursor.so* libXau.so* \
         libXdmcp.so* libpng16.so* libxcb*.so* libharfbuzz.so* \
         libcairo.so* libfreetype.so*
      ln -s ${super.bzip2.out}/lib/libbz2.so.1.0.6 libbz2.so.1.0
    )

    # Use our OpenJDK instead of Bitwig’s bundled—and commercial—one.
    rm -rf $out/libexec/lib/jre
    ln -s ${super.openjdk8}/lib/openjdk/jre $out/libexec/lib/jre

    # Bitwig’s `libx11-windowing-system.so` has several problems:
    #   • hardcodes `/usr/share/X11/xkb`, we need to redirect it,
    #   • uses eaccess(), which needs to be added to libredirect,
    #   • has some old version of libxkbcommon linked statically (ಠ_ಠ),
    #     and this old version is unable to parse our xkeyboardconfig,
    #     therefore the libxkbcommon in LD_PRELOAD…

    wrapProgram $out/libexec/bitwig-studio \
      --prefix PATH : "${binpath}" \
      --prefix LD_LIBRARY_PATH : "${libpath}" \
      --set LD_PRELOAD "${libredirect}/lib/libredirect.so:${super.libxkbcommon.out}/lib/libxkbcommon.so" \
      --set NIX_REDIRECTS /usr/share/X11/xkb=${super.xorg.xkeyboardconfig}/share/X11/xkb

    mkdir -p $out/bin
    ln -s $out/libexec/bitwig-studio $out/bin/bitwig-studio

    cp -r usr/share $out/share
  '';
}
