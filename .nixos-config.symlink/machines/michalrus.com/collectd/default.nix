{ config, lib, pkgs, ... }:

with lib;

{
  environment.systemPackages = with pkgs; [
    rrdtool  # add to PATH, too
  ];

  services.collectd = {
    enable = true;

    # Some metrics are not available to non-root users.
    user = "root";

    extraConfig = ''

      # Warning: You should set this once and then never touch it
      # again — because of the RRD format.
      Interval 10.0

      MaxReadInterval 60.0

      LoadPlugin rrdtool

      LoadPlugin cpu
      LoadPlugin load

      LoadPlugin df
      <Plugin df>
        ValuesPercentage true
      </Plugin>

      LoadPlugin ping
      <Plugin ping>
        Interval 5.0
        Timeout  4.9
        Host "10.0.1.1"
        Host "10.0.1.5"
        Host "10.0.1.11"
        Host "10.0.1.12"
        Host "10.0.1.13"
      </Plugin>

    '';
  };
}
