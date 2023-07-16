{ transcribe, writeScript, strace, lib }:

let

  randomString = "2f506693-50da-4c29-901f-e8e79f9448d9";

  dumpDRM = writeScript "transcribe-dump-drm" ''
    rm -v "$HOME"/.yeqwqauh*
    rm -v "$HOME"/.Transcribe\!7???*
    rm "$HOME/.nolistEF78.rr"
    rm -r "$HOME"/Documents/.nolistEF78.rr
    rm -r "$HOME/.frei0r-1"
    sed -re '/^InstallDate=/ d' -i "$HOME"/'.Transcribe!7'
    logDir="$HOME/.transcribe-strace-logs"
    mkdir -p "$logDir"
    logFile="$logDir/$(date -Ins).log"
    exec ${strace}/bin/strace -e trace=openat,open -o "$logFile" -- ${randomString} "$@"
  '';

in transcribe.overrideAttrs (drv: {
  postFixup = (drv.postFixup or "") + ''
    rm $out/bin/transcribe
    cp ${dumpDRM} $out/bin/transcribe
    substituteInPlace $out/bin/transcribe --replace ${randomString} $out/libexec/transcribe
  '';
  meta = removeAttrs drv.meta ["license"]; # allow unfree
})
