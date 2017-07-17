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

  # Use the gummiboot efi boot loader.
  boot.loader.gummiboot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/c5a86ac1-f1ab-4fba-90b5-ab97f41b2781";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/CC6F-8D32";
    fsType = "vfat";
  };

  fileSystems."/mnt/Windows" = {
    device = "/dev/disk/by-uuid/1E6E71196E70EB43";
    fsType = "ntfs";
    options = [ "fmask=0111" "dmask=0000" ];
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/3683496e-34b3-4bfc-9aa6-eefa5edc3438"; } ];
}
