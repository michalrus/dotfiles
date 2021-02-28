{ config, pkgs, lib, ... }:

{
  imports = [
    ../../../modules
    ../../common.nix
    ./nat.nix
    ./dns.nix
    ./openvpn.nix
  ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  # TODO: Why doesn’t `pkgs.linuxPackages_rpi3` work? Are we missing something?
  #boot.kernelPackages = pkgs.linuxPackages_rpi3;
  boot.kernelPackages = pkgs.linuxPackages_latest_ipMultipleTables;
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

  environment.systemPackages = with pkgs; [
    git
  ];

  networking.hostName = "aneta";

  services.smartd.enable = false; # FIXME: ?

  systemd.extraConfig = ''
    DefaultCPUAccounting=yes
    DefaultBlockIOAccounting=yes
    DefaultMemoryAccounting=yes
    DefaultTasksAccounting=yes
    DefaultIPAccounting=yes
  '';

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
  };

  hardware.usbWwan.enable = true;

  users = {
    mutableUsers = lib.mkForce false;
    extraUsers = {

      m = {
        hashedPassword = "$6$d3RxCdquTXL7.J7x$zRCbpql.Al1e1QAF0TO0ZkBpz6yQWjOh2HNbs0mwtrVm/BVvHEc31sUkZQk5d7dyc7yfZyyI61lXC1lXnqblL1";
        dotfiles-old.profiles = [ "base" "michalrus/base" ];
        openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
        uid = 31337;
        extraGroups = [ "wheel" ];
        isNormalUser = true;
      };

      krzyszu = {
        dotfiles-old.profiles = [ "base" "michalrus/base" ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDCQHunF/nYDmTCZrx3Bf7YynGW164RsaJ7WX9O1S3jV+uDKdSIJSvhVyexrSWGUHccBOBy78oxdX4Hcx9g7bT2oEu9wkKd1MyBODUSDScXgpSAxEbxqSHPhTLIbptfUlzac/mRLT2N5ilhe/SgFAutofxJZPYu5LyXMOGJ+CXqYB4p8TuseSzWHnpkE80+yUKe+TtrT2vgrRpT+vS+KJIHokielFUZp0yU2IMgl2AX+xgoiAKPI05t0O73bDmdRBeJW+wVwRWGtwCuhPuhDnLCalhLSzx7gm+nZE+ilC/LFHSo1XBLQYV+CNP7ecNRCRnqs/JgSQ55CnOOA1KdT3X9 krzysztof szudera@DESKTOP-8Q2GH6H"
        ];
        uid = 31338;
        extraGroups = [ "systemd-journal" ];
        isNormalUser = true;
      };

      root = {
        dotfiles-old.profiles = [ "base" ];
        openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
      };

    };
  };

}
