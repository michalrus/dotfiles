{ config, lib, pkgs, ... }:

{
  hardware = {
    #sane.extraConfig.pixma = "bjnp://10.77.4.5";
    sane.extraConfig.epson2 = "net 10.77.4.6";
  };
}
