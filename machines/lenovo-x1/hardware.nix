{
  lib,
  pkgs,
  ...
}: {
  nix.settings.max-jobs = lib.mkDefault 16;

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  boot = {
    kernelPackages = pkgs.linuxPackages_latest;

    # Use the systemd-boot EFI boot loader.
    loader.systemd-boot.enable = true;
    loader.efi.canTouchEfiVariables = true;

    ## High-DPI console
    #console.font = lib.mkDefault "${pkgs.terminus_font}/share/consolefonts/ter-u28n.psf.gz";

    initrd = {
      availableKernelModules = ["xhci_pci" "nvme" "usb_storage" "sd_mod" "sdhci_pci"];
      kernelModules = ["dm-snapshot"];
      luks.devices = {
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
    };

    kernelModules = ["kvm-intel"];
    extraModulePackages = [];

    tmp.useTmpfs = false;
    kernel.sysctl = {
      "vm.swappiness" = 60; # Let’s try the default 60 with `zramSwap`.
      # “The pernicious USB-stick stall problem” <https://lwn.net/Articles/572911/>
      "vm.dirty_background_bytes" = 16 * 1024 * 1024;
      "vm.dirty_bytes" = 16 * 1024 * 1024;
    };
  };

  # OpenGL
  nixpkgs.config.packageOverrides = pkgs: {
    intel-vaapi-driver = pkgs.intel-vaapi-driver.override {enableHybridCodec = true;};
  };
  hardware.graphics = {
    enable = true;
    extraPackages = with pkgs; [
      intel-media-driver # LIBVA_DRIVER_NAME=iHD
      intel-vaapi-driver # LIBVA_DRIVER_NAME=i965 (older but works better for Firefox/Chromium)
      libvdpau-va-gl
    ];
  };
  environment.sessionVariables.LIBVA_DRIVER_NAME = "iHD";

  services.smartd.enable = true;

  swapDevices = [
    {device = "/dev/disk/by-uuid/5e5256ed-b8a7-48c3-b815-d85792d621d6";}
  ];
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };
  systemd.oomd = {
    enable = true;
    enableRootSlice = true; # like Fedora
    enableUserSlices = true; # like Fedora
  };

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-uuid/da820fb1-2729-4438-9dec-eb2598611a44";
      fsType = "ext4";
    };

    "/boot" = {
      device = "/dev/disk/by-uuid/2ED9-B6C2";
      fsType = "vfat";
    };

    "/var" = {
      device = "/dev/disk/by-uuid/099224dd-714f-4e80-8232-dbc502159696";
      fsType = "ext4";
    };

    "/home" = {
      device = "/var/home";
      fsType = "none";
      options = ["bind"];
    };
  };
}
