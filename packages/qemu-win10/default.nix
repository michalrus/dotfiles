{
  pkgs,
  lib,
}:
pkgs.writeShellApplication rec {
  name = "qemu-win10";
  runtimeInputs = [
    pkgs.coreutils
    pkgs.qemu
    pkgs.util-linux
  ];
  text =
    builtins.replaceStrings
    ["@virtioWinIso@"]
    ["${pkgs.virtio-win.src}"]
    (builtins.readFile ./wrapper.sh);
  derivationArgs = {
    meta = {
      description = "Runs an ephemeral Windows 10 LTSC virtual machine with a shared folder";
      longDescription = ''
        This shell wrapper runs QEMU with Windows 10 LTSC inside. The base
        `system.qcow2` image stays read-only. There's also an apps image file,
        based on `system.qcow2`, which captures installed apps. Each normal run
        creates a fresh read-write `overlay-<timestamp>.qcow2` on top of the
        chosen apps image so the session is ephemeral while the app set remains
        stable. Overlays are kept on disk until you prune them. All images live
        under `~/.local/share/qemu-win10/*.qcow2`.

        The virtual machine is run without network access.

        There's also a single shared directory between the host and guest for
        file transfer.

        Use `--install-base` to perform the one-time base OS installation into
        `system.qcow2`. This mode also attaches a CD with virtio-win drivers;
        install them before shutting down. You can mount additional ISO images
        with repeated `--iso <file>` flags. If `system.qcow2` already exists,
        the wrapper exits with an error and asks you to remove it manually
        before reinstalling. In case of errors, you can always boot
        `system.qcow2` manually in QEMU to make adjustments and then continue
        with creating apps images.

        Use `--apps <name>` to select the apps image for a normal run. The name
        is required and variants live alongside `system.qcow2` as
        `apps-<name>.qcow2`. Use `--iso <file>` to attach additional ISO images.
        Use `--share-dir <dir>` to override the shared host directory.

        Use `--apps-rw <name>` to boot the selected apps image read-write, with
        no extra overlay, so you can install or update the app set in Windows.
        Use `--iso <file>` to attach additional ISO images. Use `--share-dir
        <dir>` to override the shared host directory.

        Use `--list-apps` to list available `apps-*.qcow2` images.

        Use `--prune-overlays` to remove past `overlay-*.qcow2` images.

        Use `--help` to print the usage summary.

        Examples:
        - Normal ephemeral run with a specific apps image:
          `${name} --apps finereader`
        - Install the base OS into `system.qcow2` with installer ISO:
          `${name} --install-base --iso ~/Downloads/Win10-LTSC.iso`
        - Install base OS with two installer ISOs:
          `${name} --install-base --iso ~/Downloads/Win10-LTSC.iso --iso ~/Downloads/Drivers.iso`
        - Boot a named apps image read-write to install/update apps:
          `${name} --apps-rw gamebox --iso ~/Downloads/Installer.iso`
        - Run with a custom shared directory:
          `${name} --apps finereader --share-dir ~/share/qemu`
        - Prune past overlays:
          `${name} --prune-overlays`
        - List available apps images:
          `${name} --list-apps`
        - Show help:
          `${name} --help`
      '';
      platforms = lib.platforms.linux;
    };
  };
}
