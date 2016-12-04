{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../brother.nix
  ];

  nix.maxJobs = 4;
  nix.buildCores = 4;

  boot.initrd.availableKernelModules = [ "xhci_pci" "ahci" "usb_storage" "sd_mod" "sr_mod" "rtsx_usb_sdmmc" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.kernelPackages = pkgs.linuxPackages_4_8;

  boot.initrd.luks.devices = [{
    name = "crypt";
    device = "/dev/disk/by-uuid/7595fd97-90d1-42a2-822c-8785d5a4663b";
    # allowDiscards = true; # if SSD — has security implications!
  }];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/07846ad2-f6a5-4398-8f2e-c7b2ac5d1c74";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/5482-A477";
    fsType = "vfat";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/67e88542-8ca6-4376-9547-16299cb08cb0"; }
  ];
}
