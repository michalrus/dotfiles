{ config, lib, pkgs, ... }:

{
  imports = [
    ./base-parents.nix
    ./modules/xfce.nix
  ];

  environment.systemPackages = with pkgs; [
  ];

  users.extraUsers.elzbieta = {
    hashedPassword = "$6$W/KppVZSY$.vf1jfCd6H0tOJwRwmUwJeMSkmg/MyDUlNpx3IRHWjmLpyXyg5quW0VRBX4QwGp00MIT6Nw2nODs.JhleHblz1";
    isNormalUser = true;
    description = "Elżbieta Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" "cdrom" ];
    dotfiles = [ "base" "xfce" "git-annex" "elzbietarus" ];
  };

  hardware.android.automount = let user = config.users.users.elzbieta; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Telefon";
  };

  services.xserver.displayManager = {
    lightdm.enable = false;
    auto.enable = true;
    auto.user = config.users.extraUsers.elzbieta.name;
  };
}
