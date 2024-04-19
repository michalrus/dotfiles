{ config, lib, pkgs, ... }:

{
  virtualisation.rosetta.enable = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;
  boot.initrd.availableKernelModules = [ "virtio_pci" "xhci_pci" "usb_storage" "usbhid" ];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/56b39dc9-102c-4f9d-bbcd-e9af97e7f350";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/A1F5-78F7";
    fsType = "vfat";
    options = [ "fmask=0022" "dmask=0022" ];
  };

  # Note: virtiofs is visible to anyone looking as mounted with their UID and GID as owner and group.
  # That's a bit silly security-wise, but makes sense UX-wise. We can work around by mounting it under
  # a jail accessible only to root, and then bind-mounting certain parts for different users.
  systemd.tmpfiles.rules = [ "d /run/virtiofs-jail 0700 root root -" ];

  fileSystems."/run/virtiofs-jail/share" = {
    device = "share";
    fsType = "virtiofs";
    options = [ "rw" ];
  };

  fileSystems."/home/m/Work" = {
    device = "/run/virtiofs-jail/share/Work";
    options = [ "bind" ];
  };
}
