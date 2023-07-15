{ config, lib, pkgs, ... }:

with lib;

#
# https://github.com/stevelacy/EasyCap/blob/master/documentation/usage.md
#
# nix-shell -p mplayer
# somagic-capture | mplayer -vf yadif,screenshot -demuxer rawvideo -rawvideo "pal:format=uyvy:fps=25" -aspect 4:3 -
#

let

  somagic-easycap = pkgs.callPackage (

    { stdenv, lib, fetchFromGitHub, libusb, libgcrypt }:

    let

      # TODO: maybe use this original source: https://code.google.com/archive/p/easycap-somagic-linux/
      repo = fetchFromGitHub {
        owner = "stevelacy";
        repo = "EasyCap";
        rev = "891f68851c9af8f87f03df4e3f6b89ab898b751a";
        sha256 = "0gzww6yvygnac7gk4qxvkq9j65f19vigiw4vsd273l49916v0g2j";
      };

    in stdenv.mkDerivation {
      name = "somagic-easycap";

      src = "${repo}/somagic-easycap_1.1";

      buildInputs = [ libusb libgcrypt ];

      makeFlags = [ "PREFIX=$(out)" ];

      postPatch = ''
        sed -ri 's,/lib/firmware/,'$out'/lib/firmware/,g' somagic-init.c
      '';

      preBuild = "make clean";       # Seriously?…

      installTargets = "install install-beta";

      postInstall = ''
        mkdir -p $out/lib/firmware
        cp ${repo}/somagic_firmware.bin $out/lib/firmware
        mkdir -p $out/libexec
        mv $out/bin/somagic-init $out/libexec
      '';
    }

  ) {};

in

{
  options.hardware.enableSomagicEasyCAP = mkEnableOption "Somagic EasyCAP";

  config = mkIf config.hardware.enableSomagicEasyCAP {

    environment.systemPackages = [ somagic-easycap ];

    users.extraGroups.somagic = {};

    services.udev.extraRules = ''
      # Load the Somagic firmware and reconnect with a new PID.
      ACTION=="add", ATTRS{idVendor}=="1c88", ATTRS{idProduct}=="0007", RUN+="${somagic-easycap}/libexec/somagic-init"

      # Allow group `somagic` to access the new device.
      ACTION=="add", ATTRS{idVendor}=="1c88", ATTRS{idProduct}=="003c", GROUP="somagic", MODE="0660"
    '';

    security.wrappers = builtins.listToAttrs (map (name: { inherit name; value = {
      source = "${somagic-easycap}/bin/${name}";
      setuid = false;
      setgid = true ;
      group = "somagic";
      owner = "nobody";
    }; }) ["somagic-capture" "somagic-audio-capture" "somagic-both"]);

  };
}
