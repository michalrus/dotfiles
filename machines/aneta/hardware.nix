{
  pkgs,
  lib,
  ...
}: {
  services.smartd.enable = false; # FIXME: ?

  boot = {
    loader = {
      grub.enable = false;
      generic-extlinux-compatible.enable = true;
    };

    #boot.kernelPackages = pkgs.linuxPackages_latest_ipMultipleTables;
    kernelPackages = pkgs.linuxPackages_latest;

    kernelParams = ["cma=32M" "console=ttyS1,115200n8"];

    # kernelModules = [ "gre" "ip_gre" "ip_tunnel" "ip_nat_pptp" ];
    kernel.sysctl."net.netfilter.nf_conntrack_helper" = 1;
  };

  nixpkgs = {
    overlays = [
      (_self: _super: {
        linuxPackages_latest_ipMultipleTables = pkgs.linuxPackagesFor (pkgs.linuxPackages_latest.kernel.override {
          structuredExtraConfig = with (import "${pkgs.path}/lib/kernel.nix" {
            inherit lib;
            version = null;
          }); {
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

  nix.settings = {
    max-jobs = 1;
    cores = 1;
  };

  fileSystems."/" = {
    device = "/dev/disk/by-label/NIXOS_SD";
    fsType = "ext4";
  };

  #swapDevices = [ { device = "/swapfile"; size = 1024; } ];
}
