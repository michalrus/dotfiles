_: {
  virtualisation.rosetta.enable = true;

  boot = {
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;
    initrd.availableKernelModules = ["virtio_pci" "xhci_pci" "usb_storage" "usbhid"];
  };

  # Note: virtiofs is visible to anyone looking as mounted with their UID and GID as owner and group.
  # That's a bit silly security-wise, but makes sense UX-wise. We can work around by mounting it under
  # a jail accessible only to root, and then bind-mounting certain parts for different users.
  systemd.tmpfiles.rules = ["d /run/virtiofs-jail 0700 root root -"];

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/56b39dc9-102c-4f9d-bbcd-e9af97e7f350";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/A1F5-78F7";
      fsType = "vfat";
      options = ["fmask=0022" "dmask=0022"];
    };

    "/run/virtiofs-jail/share" = {
      device = "share";
      fsType = "virtiofs";
      options = ["rw"];
    };

    "/home/m/Work" = {
      device = "/run/virtiofs-jail/share/Work";
      options = ["bind"];
    };
  };
}
