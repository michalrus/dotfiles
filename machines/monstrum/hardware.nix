{ config, lib, pkgs, ... }:

{
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  boot.initrd.availableKernelModules = [ "ahci" "xhci_pci" "ata_generic" "ehci_pci" "nvme" "usbhid" "usb_storage" "sd_mod" "sr_mod" ];
  boot.initrd.kernelModules = [ "dm-snapshot" ];
  boot.kernelModules = [ "kvm-intel" ];
  boot.extraModulePackages = [ ];

  # Use the newer `amdgpu` module for , not `radeon` for AMD FirePro W2100:
  boot.blacklistedKernelModules = [ "radeon" ];
  boot.kernelParams = [
    "radeon.si_support=0"
    "amdgpu.si_support=1"

    # Turn off physical display after N seconds:
    "consoleblank=30"

    # The default values are high, and reported as RSS <https://github.com/openzfs/zfs/issues/10251>:
    "zfs.zfs_arc_max=${toString (1 * 1024 * 1024 * 1024)}" # [B]
    "zfs.zfs_arc_min=${toString (     256 * 1024 * 1024)}" # [B]
  ];

  boot.tmp.useTmpfs = false;

  boot.initrd.luks.devices = {
    crypt-nvme0n1p2 = {
      device = "/dev/disk/by-uuid/19f0524b-dd11-404d-b88f-83b76cf9c42e";
      preLVM = true;
      allowDiscards = true; # if SSD — has security implications!
    };
    crypt-zmedia0 = {
      # /dev/disk/by-id/dm-uuid-CRYPT-LUKS2-84561e9253ed4893865f278aea4cc0db-crypt-zmedia0
      device = "/dev/disk/by-id/ata-ST2000DM006-2DM164_Z4Z9ZWAS";
      preLVM = true;
    };
  };

  boot.supportedFilesystems.zfs = true;
  networking.hostId = "21512317"; # for ZFS – has to be random between machines
  services.zfs.trim.enable = true;
  services.zfs.trim.interval = "weekly";
  services.zfs.autoScrub.enable = true;
  services.zfs.autoScrub.interval = "monthly";

  fileSystems."/" =
    { device = "/dev/disk/by-uuid/f2e763fd-3f12-49b7-901e-717fddd2fd69";
      fsType = "ext4";
    };

  fileSystems."/boot" =
    { device = "/dev/disk/by-uuid/A0B4-9020";
      fsType = "vfat";
      options = [ "fmask=0077" "dmask=0077" ];
    };

  fileSystems."/var" =
    { device = "/dev/disk/by-uuid/d70efc79-5573-4e8d-a6b9-3e5a2434b9ee";
      fsType = "ext4";
    };

  fileSystems."/var/media" =
    { device = "zmedia/media";
      fsType = "zfs";
    };

  fileSystems."/home" = {
    device = "/var/home";
    fsType = "none";
    options = ["bind"];
  };

  fileSystems."/var/home/k/Downloads"  = { device = "/var/media/downloads/k";  fsType = "none"; options = ["bind"]; };
  fileSystems."/var/home/m/Downloads"  = { device = "/var/media/downloads/m";  fsType = "none"; options = ["bind"]; };
  fileSystems."/var/home/km/Downloads" = { device = "/var/media/downloads/km"; fsType = "none"; options = ["bind"]; };

  fileSystems."/var/home/k/Torrents"  = { device = "/var/media/torrents"; fsType = "none"; options = ["bind" "ro"]; };
  fileSystems."/var/home/m/Torrents"  = { device = "/var/media/torrents"; fsType = "none"; options = ["bind" "ro"]; };
  fileSystems."/var/home/km/Torrents" = { device = "/var/media/torrents"; fsType = "none"; options = ["bind" "ro"]; };

  swapDevices = [ ];

  # Enables DHCP on each ethernet and wireless interface. In case of scripted networking
  # (the default) this is the recommended approach. When using systemd-networkd it's
  # still possible to use this option, but it's recommended to use it in conjunction
  # with explicit per-interface declarations with `networking.interfaces.<interface>.useDHCP`.
  networking.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp0s25.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp15s0.useDHCP = lib.mkDefault true;
  # networking.interfaces.enp16s0.useDHCP = lib.mkDefault true;

  nixpkgs.hostPlatform = lib.mkDefault "x86_64-linux";
  hardware.cpu.intel.updateMicrocode = lib.mkDefault config.hardware.enableRedistributableFirmware;
}
