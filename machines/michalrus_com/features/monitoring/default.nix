{pkgs, ...}: let
  influxdbCollecdPort = 25826;
  influxdb10 =
    (pkgs.influxdb.overrideAttrs (_: {
      patches = [./influxdb-add_increase.patch];
    })).bin
    // {outputs = ["bin"];};
in {
  services.influxdb = {
    enable = true;
    package = influxdb10;
    extraConfig = {
      collectd = [
        {
          enabled = true;
          typesdb = "${pkgs.collectd}/share/collectd/types.db";
          database = "collectd";
          port = influxdbCollecdPort;
        }
      ];
    };
  };

  services.collectd = {
    enable = true;

    # Some metrics are not available to non-root users.
    user = "root";

    extraConfig = ''
      Interval 10.0
      MaxReadInterval 60.0

      # Send data to InfluxDB collectd service.
      LoadPlugin network
      <Plugin network>
        Server "localhost" "${builtins.toString influxdbCollecdPort}"
      </Plugin>

      LoadPlugin cpu
      LoadPlugin load
      LoadPlugin df
      LoadPlugin disk
      LoadPlugin entropy
      LoadPlugin interface
      LoadPlugin memory
      LoadPlugin processes
      LoadPlugin swap
      LoadPlugin uptime
      LoadPlugin users
      LoadPlugin vmem

      LoadPlugin iptables
      <Plugin iptables>
        Chain "filter" "nixos-fw-comments-in"
        Chain "filter" "nixos-fw-comments-out"
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
