super: self:

let

  script = super.writeScript "script" ''
    #!/bin/sh
    cd "$HOME"
    as="$HOME/.config/git-annex/autostart"
    pwd
    [ -f "$as" ] && cd "$(head -n 1 "$as")"
    pwd
    exec /run/current-system/sw/bin/git-annex webapp
  '';

  launcher = super.writeText "launcher" ''
    [Desktop Entry]
    Name=git-annex webapp
    Exec=${script}
    Icon=git-annex
    Type=Application
    Terminal=false
  '';

  logo = super.fetchurl {
    url = https://git-annex.branchable.com/logo.svg;
    sha256 = "0wrzllbb39l6zkmf7r0migwqdvcinjkhln24953cxyv1g6jrsarh";
  };

in

super.stdenv.mkDerivation {
  name = "git-annex-desktop";

  phases = [ "installPhase" ];

  installPhase = ''
    mkdir -p $out/share/applications
    mkdir -p $out/share/icons/hicolor/scalable/apps
    ln -s ${launcher} $out/share/applications/git-annex.desktop
    ln -s ${logo} $out/share/icons/hicolor/scalable/apps/git-annex.svg
  '';
}
