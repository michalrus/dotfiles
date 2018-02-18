{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../mother.nix
  ];

  services.xserver.synaptics = {
    maxSpeed = "5.0";
    accelFactor = "0.025";
  };

  services.xserver.useGlamor = true;

  nix.maxJobs = 3;
  nix.buildCores = 3;

  boot.initrd.availableKernelModules = [ "ehci_pci" "ahci" "xhci_pci" "usb_storage" "sd_mod" "sr_mod" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  boot.kernel.sysctl."vm.swappiness" = 5; # Use swap more reluctantly.

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/disk/by-id/ata-SAMSUNG_HM321HI_S26VJ9AB901679";
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/a9ad63c9-a869-47da-85cd-0eb10a2eb04f";
    fsType = "ext4";
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/a5e3ec45-ef4b-4a75-94c5-bdd9476c5484"; } ];
}
