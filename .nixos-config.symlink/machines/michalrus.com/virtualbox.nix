{ config, pkgs, ... }:

{

  imports = [
    ./default.nix
  ];

  boot.loader.grub = {
    enable = true;
    version = 2;
    device = "/dev/sda";
  };

}
