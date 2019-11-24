{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.hardware.android.automount;
in

{
  options = {
    hardware.android.automount = {
      enable = mkEnableOption "automounting MTP devices with jmtpfs";
      user = mkOption {
        type = types.str;
        default = "root";
        description = "Which user to mount as.";
      };
      point = mkOption {
        type = types.path;
        default = "/media/Android";
        description = "Where to mount.";
      };
    };
  };

  config = mkMerge [
    {
      environment.systemPackages = with pkgs; [
        jmtpfs
        libmtp
      ];

      services.udev.packages = [ pkgs.android-udev-rules ];
    }

    (mkIf cfg.enable {
      services.udev.extraRules = let
        mount = pkgs.writeScript "android-mount" ''
          #!/bin/sh

          # Check if there’s anything really available and exit cleanly if not.
          numdev="$( ( ${pkgs.jmtpfs}/bin/jmtpfs -l || true ) | wc -l )"
          [ "$numdev" -gt 1 ] || exit 0

          uid="$(id -u "${cfg.user}")"
          gid="$(id -g "${cfg.user}")"
          mkdir -p "${cfg.point}"
          chown "$uid:$gid" "${cfg.point}"
          ${config.systemd.package}/bin/systemd-run ${pkgs.jmtpfs}/bin/jmtpfs -f \
            -o "allow_other,auto_unmount,uid=$uid,gid=$gid" \
            "${cfg.point}"
        '';

        unmount = pkgs.writeScript "android-unmount" ''
          #!/bin/sh
          ${pkgs.utillinux}/bin/umount --force "${cfg.point}"
          rmdir "${cfg.point}"
        '';

        in ''
          SUBSYSTEM=="usb", ATTR{bInterfaceNumber}=="00", ACTION=="add", ENV{android_mtp}="yes", RUN+="${mount}"
          SUBSYSTEM=="usb", ENV{android_mtp}=="yes", ACTION=="remove", RUN+="${unmount}"
        '';
    })
  ];
}
