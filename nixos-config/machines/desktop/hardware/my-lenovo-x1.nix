{ config, lib, pkgs, ... }:

{
  imports =
    [
    ../base-mine.nix
    #../modules/musnix.nix
    #../modules/android-dev.nix
    #../modules/chwalecice.nix
    ];

  boot.initrd.availableKernelModules = [ "xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # total 0
  # lrwxrwxrwx 1 root root 10 Dec 19 13:34 099224dd-714f-4e80-8232-dbc502159696 -> ../../dm-4
  # lrwxrwxrwx 1 root root 15 Dec 19 13:27 2ED9-B6C2 -> ../../nvme0n1p1
  # lrwxrwxrwx 1 root root 10 Dec 19 13:34 5e5256ed-b8a7-48c3-b815-d85792d621d6 -> ../../dm-3
  # lrwxrwxrwx 1 root root 15 Dec 19 13:32 624b00a5-a8dc-442c-aa0a-db1388ed3746 -> ../../nvme1n1p1
  # lrwxrwxrwx 1 root root 15 Dec 19 13:31 b57dc4b7-0962-42bf-b650-7302166077ab -> ../../nvme0n1p2
  # lrwxrwxrwx 1 root root 10 Dec 19 13:34 da820fb1-2729-4438-9dec-eb2598611a44 -> ../../dm-2

  boot.initrd.luks.devices = {
    nvme-crypt0 = {
      device = "/dev/disk/by-uuid/b57dc4b7-0962-42bf-b650-7302166077ab";
      preLVM = true;
      allowDiscards = true; # if SSD — has security implications!
    };
    nvme-crypt1 = {
      device = "/dev/disk/by-uuid/624b00a5-a8dc-442c-aa0a-db1388ed3746";
      preLVM = true;
      allowDiscards = true; # if SSD — has security implications!
    };
  };

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/da820fb1-2729-4438-9dec-eb2598611a44";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/2ED9-B6C2";
      fsType = "vfat";
    };

  fileSystems."/var" =
    { device = "/dev/disk/by-uuid/099224dd-714f-4e80-8232-dbc502159696";
      fsType = "ext4";
    };

  fileSystems."/home" = {
    device = "/var/home";
    fsType = "none";
    options = [ "bind" ];
  };


  swapDevices =
    [ { device = "/dev/disk/by-uuid/5e5256ed-b8a7-48c3-b815-d85792d621d6"; }
    ];

  nix.maxJobs = lib.mkDefault 16;
  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";
  # High-DPI console
  console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;







  time.timeZone = "Europe/Warsaw";
  boot.kernel.sysctl."vm.swappiness" = lib.mkForce 1; # Let’s try this.
  boot.kernelPackages = pkgs.linuxPackages_5_10;
}
