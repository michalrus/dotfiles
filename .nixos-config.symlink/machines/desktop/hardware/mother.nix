{ config, lib, pkgs, ... }:

{
  imports = [
    <nixpkgs/nixos/modules/installer/scan/not-detected.nix>
    ../base-parents.nix
  ];

  users.extraUsers.elzbieta = {
    hashedPassword = "$6$W/KppVZSY$.vf1jfCd6H0tOJwRwmUwJeMSkmg/MyDUlNpx3IRHWjmLpyXyg5quW0VRBX4QwGp00MIT6Nw2nODs.JhleHblz1";
    isNormalUser = true;
    description = "Elżbieta Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" ];
  };

  services.xserver.displayManager = {
    lightdm.enable = false;
    auto.enable = true;
    auto.user = config.users.extraUsers.elzbieta.name;
  };

  nix.maxJobs = 4;
  nix.buildCores = 4;

  boot.initrd.availableKernelModules = [ "uhci_hcd" "ehci_pci" "ahci" "sd_mod" ];
  boot.kernelModules = [ ];
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.blacklistedKernelModules = [ "toshiba_wmi" ]; # why is this loaded at all?
  boot.extraModulePackages = [];

  boot.kernel.sysctl."vm.swappiness" = 5; # Use swap more reluctantly.

  nixpkgs.config.packageOverrides = super: {
    stdenv = super.stdenv // {
      platform = super.stdenv.platform // {
        kernelExtraConfig = ''
          DRM_GMA600 y
          DRM_GMA3600 y
        '';
      };
    };
  };

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/disk/by-id/ata-ST320LT020-9YG142_W044HZ0X";
#    extraEntries = ''
#      menuentry "Windows 7 Ultimate" {
#        insmod part_msdos
#        insmod ntfs
#        set root='(hd0,msdos2)'
#        chainloader +1
#      }
#    '';
  };

  fileSystems."/" = {
    device = "/dev/disk/by-uuid/a9ad63c9-a869-47da-85cd-0eb10a2eb04f";
    fsType = "ext4";
  };

  fileSystems."/mnt/Windows" = {
    device = "/dev/disk/by-uuid/32C0AE4CC0AE165F";
    fsType = "ntfs";
    options = [ "fmask=0133" "dmask=0022" ];
  };

  swapDevices = [ { device = "/var/swap"; } ];
}
