{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../father.nix
  ];

  nix.maxJobs = 4;
  nix.buildCores = 4;

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "sd_mod" ];
  boot.kernelModules = [ ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.blacklistedKernelModules = [ "toshiba_wmi" ]; # why is this loaded at all?
  boot.extraModulePackages = [];

  boot.kernel.sysctl."vm.swappiness" = 1; # We’ve got only 1.5G here, and HDD is so slooow.

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-WDC_WD2500BEVT-75ZCT2_WD-WXB0AA950072";

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/da4295f4-1ccb-4933-b123-a72bf24cc371";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/5552313a-e149-4b79-a833-c17b5e3dc03d";
    fsType = "ext4";
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/26e21201-3c99-4dca-876b-fb6de48d5aa2";
    fsType = "ext4";
  };

  fileSystems."/home" = {
    device = "/var/home";
    fsType = "none";
    options = [ "bind" ];
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/874a388b-a8d0-4143-af41-473b7e30bd45"; } ];
}
