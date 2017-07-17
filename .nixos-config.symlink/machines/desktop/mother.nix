{ config, lib, pkgs, ... }:

{
  imports = [
    ./base-parents.nix
    ./modules/xfce.nix
  ];

  users.extraUsers.elzbieta = {
    hashedPassword = "$6$W/KppVZSY$.vf1jfCd6H0tOJwRwmUwJeMSkmg/MyDUlNpx3IRHWjmLpyXyg5quW0VRBX4QwGp00MIT6Nw2nODs.JhleHblz1";
    isNormalUser = true;
    description = "Elżbieta Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" ];
  };

  # Temporarily?
  users.extraUsers.robert = {
    hashedPassword = "$6$rcYySsCDE$X/ilZ3Z4/3dUQ0pPXwnStOQQAsGuoCNY26/29oA4vY6gj.9ZpFYnpaiCUXl4w4sEBdtzqze42LePiIFx51cmM1";
    isNormalUser = true;
    description = "Robert Rus";
    extraGroups = [ "scanner" "networkmanager" ];
  };

  hardware.android.automount = let user = config.users.users.elzbieta; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Telefon";
  };

  services.xserver.displayManager = {
    # Temporarily?
    lightdm.enable = true;
    #lightdm.enable = false;
    #auto.enable = true;
    #auto.user = config.users.extraUsers.elzbieta.name;
  };
}
