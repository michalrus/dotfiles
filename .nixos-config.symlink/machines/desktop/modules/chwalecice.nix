{ config, lib, pkgs, ... }:

{
  hardware = {
    sane.extraConfig.pixma = "bjnp://10.0.1.5";
  };
}
