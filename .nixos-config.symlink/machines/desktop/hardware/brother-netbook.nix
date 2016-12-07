{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../brother.nix
  ];

  nix.maxJobs = 1;
  nix.buildCores = 1;

  # Broadcom drivers & firmware.
  nixpkgs.config.allowUnfree = true;
  networking.enableB43Firmware = true;

  # Use the GRUB 2 boot loader.
  boot.loader.grub.enable = true;
  boot.loader.grub.version = 2;
  boot.loader.grub.device = "/dev/disk/by-id/ata-SAMSUNG_HM500JI_S20CJ9BB213689";

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "usb_storage" "sd_mod" ];
  boot.kernelModules = [ "wl" ];
  boot.extraModulePackages = [ config.boot.kernelPackages.broadcom_sta ];

  boot.initrd.luks.devices = [{
    name = "crypt";
    device = "/dev/disk/by-uuid/46113ead-b9c2-4fc5-872a-146065518d02";
    # allowDiscards = true; # if SSD — has security implications!
  }];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/41bd14e2-431f-4c58-90cc-fd0c8ccfbf7b";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/1bf8e828-fbf1-49d1-a924-534c7f8ac8c7";
    fsType = "ext4";
  };

  swapDevices = [
    { device = "/dev/disk/by-uuid/d482a36d-68eb-4f1f-8067-0213906ba9fc"; }
  ];
}
