{ config, pkgs, lib, ... }:

{
  imports = [
    ../../modules
    ../desktop/modules/openvpn.nix
    ../common.nix
    ./router-chwalecice--nat.nix
  ];

  # Let’s try not adding this high-metric route to 10.77.4.0/24 via tun0.
  # We already have one without metric (so used always?) via wlan0, obviously.
  # For some reason *sometimes* packages from other computers in wlan0 cannot get to other computers in wlan0 (only this router responds to pings then).
  # I dunno why, I dunno how to reproduce.
  # Let’s then try NOT pulling this `10.77.4.0/24 via tun0 metric 1200` route from openvpn.
  services.openvpn.servers.michalrus_com.config = ''
    route-nopull
  '';

  nixpkgs.overlays = [ (import ../../overlays) ];

  boot.loader.grub.enable = false;
  boot.loader.generic-extlinux-compatible.enable = true;

  boot.kernelPackages = pkgs.linuxPackages_latest;

  nix.binaryCaches = lib.mkForce [ "http://nixos-arm.dezgeg.me/channel" ];
  nix.binaryCachePublicKeys = [ "nixos-arm.dezgeg.me-1:xBaUKS3n17BZPKeyxL4JfbTqECsT+ysbDJz29kLFRW0=%" ];

  boot.kernelParams = ["console=ttyS0,115200n8" "console=ttymxc0,115200n8" "console=ttyAMA0,115200n8" "console=ttyO0,115200n8" "console=ttySAC2,115200n8" "console=tty0"];

  hardware.enableRedistributableFirmware = true;

  nix.maxJobs = 3;
  nix.buildCores = 3;

  users = {
    users.root = {
      hashedPassword = "$6$o1OW/gg.pZZPr$NKD3sSEg45yLNpT0caXw2bZqWDsHXRvPko4XWGGBcUjyWa0wkw9g6n55bpjwAhkiys3chI6rr6MPVL1rtCJpj/"; # for serial TTY login only
      dotfiles.profiles = [ "base" ];
      openssh.authorizedKeys.keyFiles = [ ../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
    };
  };

  environment.systemPackages = with pkgs; [];

  services.smartd.enable = false; # FIXME: ?

  services.openssh = {
    enable = true;
    passwordAuthentication = false;
    challengeResponseAuthentication = false;
  };

  fileSystems = {
    "/boot" = {
      device = "/dev/disk/by-label/NIXOS_BOOT";
      fsType = "vfat";
    };
    "/" = {
      device = "/dev/disk/by-label/NIXOS_SD";
      fsType = "ext4";
    };
  };

  # swapDevices = [ { device = "/swapfile"; size = 1024; } ];
}
