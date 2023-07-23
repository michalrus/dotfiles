{ config, pkgs, lib, ... }:

{
  services.smartd.enable = false; # FIXME: ?

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # TODO: Why doesn’t `pkgs.linuxPackages_rpi3` work? Are we missing something?
  #boot.kernelPackages = pkgs.linuxPackages_rpi3;
  #boot.kernelPackages = pkgs.linuxPackages_latest_ipMultipleTables; # FIXME: this overlays below don’t work with flakes
  boot.kernelPackages = pkgs.linuxPackages_latest;
  boot.kernelParams = [ "cma=32M" "console=ttyS1,115200n8" ];

  boot.kernelModules = [ "gre" "ip_gre" "ip_tunnel" "ip_nat_pptp" ];
  boot.kernel.sysctl."net.netfilter.nf_conntrack_helper" = 1;

  nixpkgs = {
    overlays = [
      (self: super: {
        linuxPackages_latest_ipMultipleTables = pkgs.linuxPackagesFor (pkgs.linuxPackages_latest.kernel.override {
          structuredExtraConfig = with (import "${pkgs.path}/lib/kernel.nix" { inherit lib; version = null; });
          {
            IP_ADVANCED_ROUTER = yes;
            IP_MULTIPLE_TABLES = yes;
            IP_ROUTE_MULTIPATH = yes;
            IP_PNP = yes;
            NET_IPGRE_BROADCAST = yes;
            IP_MROUTE = yes;
            IP_MROUTE_MULTIPLE_TABLES = yes;
          };
        });
      })
    ];
  };

  # boot.kernelPatches = [ {
  #   name = "arm64-multiple-routing-tables";
  #   patch = null;
  #   extraConfig = ''
  #     IP_ROUTE_MULTIPATH y
  #     IP_MULTIPLE_TABLES y
  #     IP_MROUTE_COMMON y
  #     IP_MROUTE y
  #     IP_MROUTE_MULTIPLE_TABLES y
  #     IPV6_MULTIPLE_TABLES y
  #     IPV6_MROUTE y
  #     IPV6_MROUTE_MULTIPLE_TABLES y
  #   '';
  # } ];

  hardware.enableRedistributableFirmware = true;
  #hardware.firmware = [ pkgs.wireless-regdb ];
  #services.udev.packages = [ pkgs.crda ];
  #boot.extraModprobeConfig = ''
  #  options cfg80211 ieee80211_regdom=EU
  #'';

  nix.maxJobs = 1;
  nix.buildCores = 1;

  fileSystems = {
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  #swapDevices = [ { device = "/swapfile"; size = 1024; } ];
}
