{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../father.nix
  ];

  nix.maxJobs = 2;
  nix.buildCores = 2;

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "usb_storage" "usbhid" "sd_mod" "rtsx_pci_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" "wl" ];

  nixpkgs.config.allowUnfree = true; # For the Broadcom driver below...
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  boot.kernel.sysctl."vm.swappiness" = 5; # Use swap more reluctantly.

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/sda";

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c3f59fda-d456-40ce-92cb-972ad08d3095";
    fsType = "ext4";
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/6738da51-edcc-46be-93a5-6ea6076fb280"; } ];
}
