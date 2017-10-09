super: self:

let

  randomString = "2f506693-50da-4c29-901f-e8e79f9448d9";

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

super.transcribe.overrideDerivation (oldAttrs: {

  fixupPhase = ''
    rm $out/bin/transcribe
    cp ${dumpDRM} $out/bin/transcribe
    substituteInPlace $out/bin/transcribe --replace ${randomString} $out/libexec/transcribe
  '';

})
