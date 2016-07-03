{ config, pkgs, ... }:

{
  imports = [
    ./modules
    ./pkgs
    ./local
    ./common.nix
    ./hardware-configuration.nix
  ];

  nix.useChroot = true;   # use useSandbox from common.nix when in stable!

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

  networking.hostName = "michalrus_com";

  time.timeZone = "UTC";

  environment.systemPackages = with pkgs; [
  ];

  programs = {
    ssh.startAgent = false;
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = "no";
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
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
