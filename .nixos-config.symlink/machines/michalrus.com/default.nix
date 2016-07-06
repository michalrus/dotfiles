{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../pkgs
    ../../local
    ../../common.nix
    ../../hardware-configuration.nix
    ./web
  ];

  nix.useChroot = true;   # use useSandbox from common.nix when in stable!

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  networking.hostName = "michalrus_com";

  networking.firewall.allowedTCPPorts = [
    113  # identd
  ];

  time.timeZone = "UTC";

  environment.systemPackages = with pkgs; [
    weechat
  ];

  programs = {
    ssh.startAgent = false;
  };

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # this fix is not yet in stable…
    oidentd = super.oidentd.overrideDerivation(oldAttrs: { CFLAGS = [ "--std=gnu89" ]; });
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    oidentd.enable = true;

    bitlbee = {
      enable = true;
      plugins = with pkgs; [ bitlbee-facebook ];
    };
  };

  users = {
    # Remember to `sudo loginctl enable-linger m`, or screen sessions won’t last.
    # https://github.com/NixOS/nixpkgs/issues/3702
    extraUsers.m = {
      isNormalUser = true;
      hashedPassword = "$6$veGcmDHbAC4pOsm$/Me.dvpaWQn3YwQEivKd3iL1uXVLV3fr3NlM4UCMjIXDj3jvexSmPq1zZRW.k.rsXg8kRovUIXTB1iLSRYgM30";
      description = "Michal Rus";
      extraGroups = [ "wheel" ];
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";
}
