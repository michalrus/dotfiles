super: self:

let

  randomString = "2f506693-50da-4c29-901f-e8e79f9448d9";

  libpath = super.lib.makeLibraryPath (with super; [
    stdenv.cc.cc glib gtk2 atk pango cairo gdk_pixbuf alsaLib
    xorg.libX11 xorg.libSM libpng12 gstreamer gst_plugins_base zlib
  ]);

  dumpDRM = super.writeScript "transcribe-dump-drm" ''
    rm "$HOME/.yeqwqauh"
    rm "$HOME/.yeqwqauhu75qeA"
    rm "$HOME/.Transcribe!7kjIQ1y"
    rm "$HOME/.nolistEF78.rr"
    rm -r "$HOME/.frei0r-1"
    sed -re '/^InstallDate=/ d' -i "$HOME"/'.Transcribe!7'
    logDir="$HOME/.transcribe-strace-logs"
    mkdir -p "$logDir"
    logFile="$logDir/$(date -Ins).log"
    exec ${super.strace}/bin/strace -e open -o "$logFile" -- ${randomString} "$@"
    '';

in

super.stdenv.mkDerivation {
  name = "transcribe-8.40";

  src = super.fetchurl {
    url = "https://www.seventhstring.com/xscribe/downlinux64_old/xsc64setup.tar.gz";
    sha256 = "0fqs0f2p9kbp1skgqm5sgcq9xxf6rccbm22fvka8qmpvxpsxpcbs";
  };

  buildInputs = with super; [ makeWrapper gst_plugins_base gst_plugins_good
    gst_plugins_bad gst_plugins_ugly gst_ffmpeg ];

  dontPatchELF = true;

  installPhase = ''
    mkdir -p $out/bin $out/bin.original $out/share/doc
    cp transcribe $out/bin.original
    cp xschelp.htb readme_gtk.html $out/share/doc
    cp -r gtkicons $out/share/icons

    ln -s $out/share/doc/xschelp.htb $out/bin.original

    patchelf \
      --set-interpreter $(cat ${super.stdenv.cc}/nix-support/dynamic-linker) \
      $out/bin.original/transcribe

    wrapProgram $out/bin.original/transcribe \
      --prefix GST_PLUGIN_SYSTEM_PATH : "$GST_PLUGIN_SYSTEM_PATH" \
      --prefix LD_LIBRARY_PATH : "${libpath}"

    cp ${dumpDRM} $out/bin/transcribe
    substituteInPlace $out/bin/transcribe --replace ${randomString} $out/bin.original/transcribe
    '';
}
