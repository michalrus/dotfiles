{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../father.nix
  ];

  services.xserver.useGlamor = true;

  nix.maxJobs = 4;
  nix.buildCores = 4;

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "ehci_pci" "usb_storage" "usbhid" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" "wl" ];

  nixpkgs.config.allowUnfree = true; # For the Broadcom driver below...
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  boot.kernel.sysctl."vm.swappiness" = 5; # Use swap more reluctantly.

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c3f59fda-d456-40ce-92cb-972ad08d3095";
    fsType = "ext4";
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/f6ec3e74-2409-48a8-88c1-155f36ef9b84"; } ];
}
