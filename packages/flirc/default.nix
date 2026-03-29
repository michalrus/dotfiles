{
  lib,
  writeShellApplication,
  bubblewrap,
  path,
  stdenv,
}: let
  unfree-nixpkgs = import path {
    inherit (stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };
  unsafe = unfree-nixpkgs.flirc;
  bwrap = writeShellApplication {
    name = "flirc_util";
    text = ''
      dev_args=()
      for dev in /dev/hidraw*; do
        [ -e "$dev" ] && dev_args+=(--dev-bind "$dev" "$dev")
      done
      [ -d /dev/bus/usb ] && dev_args+=(--dev-bind /dev/bus/usb /dev/bus/usb)
      [ -d /sys ]         && dev_args+=(--ro-bind /sys /sys)
      [ -d /run/udev ]    && dev_args+=(--ro-bind /run/udev /run/udev)
      exec ${lib.getExe bubblewrap} \
        --unshare-all --new-session --die-with-parent \
        --clearenv \
        --proc /proc \
        --dev /dev \
        "''${dev_args[@]}" \
        --tmpfs /tmp \
        --ro-bind /nix/store /nix/store \
        ${unsafe}/bin/flirc_util "$@"
    '';
  };
in
  bwrap // {passthru.udevDir = "${unsafe}/lib/udev";}
