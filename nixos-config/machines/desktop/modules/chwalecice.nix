{ config, lib, pkgs, ... }:

{
  hardware = {
    sane.extraConfig.pixma = "bjnp://10.77.4.5";
  };
}
