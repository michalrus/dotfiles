{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../base-mine.nix
    #../modules/musnix.nix
    #../modules/android-dev.nix
    ../modules/chwalecice.nix
  ];

  nix.maxJobs = 4;
  nix.buildCores = 4;

  time.timeZone = "Europe/Warsaw";

  boot.initrd.availableKernelModules = [ "xhci_pci" "ehci_pci" "ahci" "usb_storage" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];
  boot.blacklistedKernelModules = [ "radeon" "amdgpu" ]; # I’m getting some errors in journal.

  boot.kernel.sysctl."vm.swappiness" = lib.mkForce 1; # Let’s try this.

  services.xserver.useGlamor = true;

  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.luks.devices = [{
    name = "crypt";
    device = "/dev/disk/by-uuid/f671aaa7-2b5c-44e3-9c83-6997edb4bcc4";
    allowDiscards = true; # if SSD — has security implications!
  }];

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/99f04383-1326-4321-af74-995070736843";
    fsType = "ext4";
  };

  fileSystems."/var" = {
    device = "/dev/disk/by-uuid/1726aad0-192d-4df3-b90f-997708979298";
    fsType = "ext4";
  };

  fileSystems."/boot" = {
    device = "/dev/disk/by-uuid/DD88-498B";
    fsType = "vfat";
  };

  fileSystems."/home" = {
    device = "/var/home";
    fsType = "none";
    options = [ "bind" ];
  };

  swapDevices = [ { device = "/dev/disk/by-uuid/b03c839e-4f76-4ef5-bcc0-bc26d98fe167"; } ];
}
