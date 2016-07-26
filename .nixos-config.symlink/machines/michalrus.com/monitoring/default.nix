{ config, lib, pkgs, ... }:

with lib;

let
  influxdbCollecdPort = 25826;
in

{

  services.influxdb = {
    enable = true;
    extraConfig = {
      collectd = {
        enabled = true;
        port = influxdbCollecdPort;
        database = "collectd";
      };
    };
  };

  services.collectd = {
    enable = true;

    # Some metrics are not available to non-root users.
    user = "root";

    extraConfig = ''

      # Warning: You should set this once and then never touch it
      # again — because of the RRD format.
      Interval 10.0

      MaxReadInterval 60.0

      # Send data to InfluxDB collectd service.
      LoadPlugin network
      <Plugin network>
        Server "localhost" "${toString influxdbCollecdPort}"
      </Plugin>

      LoadPlugin cpu
      LoadPlugin load

      LoadPlugin df
      <Plugin df>
        ValuesPercentage true
      </Plugin>

      LoadPlugin memory
      Plugin memory

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

      LoadPlugin vmem
      <Plugin vmem>
        Verbose false
      </Plugin>

    '';
  };

}
