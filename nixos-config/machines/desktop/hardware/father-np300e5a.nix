{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../father.nix
  ];

  nix.maxJobs = 3;
  nix.buildCores = 3;

  boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.kernel.sysctl."vm.swappiness" = 5; # Use swap more reluctantly.

  boot.initrd.luks.devices = [{
    name = "crypt";
    device = "/dev/disk/by-uuid/31ad506e-75e9-4c07-bedd-362c8a50aa5b";
    # allowDiscards = true; # if SSD — has security implications!
  }];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/1f9cf4c0-7e3f-4287-882e-facaf27d1297";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/498ebb19-9b2f-4b06-bcf6-3d299ff90265";
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/77935e44-8fff-48a5-be06-8eb20f5ae0c7"; }
  ];

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-SAMSUNG_HM321HI_S26VJ9AB901679";
}
