{
  lib,
  pkgs,
  ...
}: {
  hardware = {
    enableRedistributableFirmware = lib.mkDefault true;
    cpu.intel.updateMicrocode = lib.mkDefault true;
    i2c.enable = true;
    rasdaemon.enable = true;
  };

  systemd.oomd = {
    enable = true;
    enableRootSlice = true; # like Fedora
    enableUserSlices = true; # like Fedora
  };

  services = {
    fwupd.enable = true;
    smartd.enable = true;
    thermald.enable = true;
  };

  environment.systemPackages = with pkgs; [
    acpi
    dmidecode
    ethtool
    hdparm
    hwinfo
    i2c-tools
    intel-gpu-tools
    linuxPackages.cpupower
    linuxPackages.turbostat
    lm_sensors
    lshw
    nvme-cli
    pciutils
    powertop
    smartmontools
    tlp # for `tlp-stat -b`
    usbutils
  ];
}
