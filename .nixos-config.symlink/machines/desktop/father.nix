{ config, lib, pkgs, ... }:

{
  imports = [
    ./base-parents.nix
    ./modules/kde.nix
  ];

  virtualisation.virtualbox.host.enable = true;

  environment.systemPackages = with pkgs; [
    calibre
    michalrus.xpad
    nixos-unstable.ltris
    pcmanfm
  ];

  users.extraUsers.robert = {
    hashedPassword = "$6$rcYySsCDE$X/ilZ3Z4/3dUQ0pPXwnStOQQAsGuoCNY26/29oA4vY6gj.9ZpFYnpaiCUXl4w4sEBdtzqze42LePiIFx51cmM1";
    isNormalUser = true;
    description = "Robert Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" "vboxusers" "cdrom" ];
  };

  hardware.android.automount = let user = config.users.users.robert; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Telefon";
  };
}
