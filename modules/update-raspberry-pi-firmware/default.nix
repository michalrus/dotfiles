{
  lib,
  pkgs,
  config,
  ...
}: let
  cfg = config.boot.loader.raspberryPiFirmware;

  sdCardSystem = import "${pkgs.path}/nixos/lib/eval-config.nix" {
    inherit (pkgs.stdenv.hostPlatform) system;
    modules = ["${pkgs.path}/nixos/modules/installer/sd-card/sd-image-aarch64.nix"];
  };

  extraConfigTxt = pkgs.writeText "extra-config.txt" ("\n"
    + ''
      ${
        if cfg.version == 3
        then ''
          # Or else we'll get garbled U-Boot output, see:
          # - <https://github.com/raspberrypi/linux/issues/4123>
          # - <https://nixos.wiki/wiki/NixOS_on_ARM/Raspberry_Pi_3#Early_boot>
          core_freq=250
        ''
        else ""
      }

      ${cfg.extraConfigTxt}
    '');

  freshFirmware = pkgs.runCommand "rpi-firmware-partition" {} ''
    mkdir -p $NIX_BUILD_TOP/firmware
    ${sdCardSystem.config.sdImage.populateFirmwareCommands}
    mkdir -p $out
    cp -r $NIX_BUILD_TOP/firmware/. $out/.

    chmod +w $out/config.txt
    cat ${extraConfigTxt} >>$out/config.txt
  '';

  mountPoint = "/boot/firmware";
  backupDir = "/var/lib/rpi-firmware-backup";

  activationScript = pkgs.writeShellScript "update-rpi-firmware" ''
    set -euo pipefail
    if ${pkgs.diffutils}/bin/diff -r ${lib.escapeShellArg mountPoint} ${freshFirmware} 1>/dev/null 2>/dev/null ; then
      :
    else
      echo >&2 'updating Raspberry Pi firmware in '${lib.escapeShellArg mountPoint}'...'
      backupDir=${lib.escapeShellArg backupDir}/"$(date -Ins)"
      mkdir -p "$backupDir"
      cp -rp ${lib.escapeShellArg mountPoint}/. "$backupDir"/
      rm -rf ${lib.escapeShellArg mountPoint}/*
      cp -rp ${freshFirmware}/. ${lib.escapeShellArg mountPoint}/
    fi
  '';
in {
  options.boot.loader.raspberryPiFirmware = {
    version = lib.mkOption {
      type = lib.types.enum [0 1 2 3 4];
    };

    extraConfigTxt = lib.mkOption {
      default = "";
      type = lib.types.lines;
    };
  };

  config = {
    fileSystems.${mountPoint} = {
      device = "/dev/disk/by-label/FIRMWARE";
      fsType = "vfat";
    };

    # They’re ordered by name after satisfying dependencies:
    system.activationScripts.update-rpi-firmware = {
      deps = ["binsh" "wrappers" "var"];
      text = toString activationScript;
    };
  };
}
