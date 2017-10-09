{ config, pkgs, ... }:

{

  imports = [
    <nixpkgs/nixos/modules/virtualisation/amazon-image.nix>
    ./default.nix
  ];

  ec2.hvm = true;

}
