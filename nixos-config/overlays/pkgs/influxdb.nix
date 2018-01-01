self: super:

{

  influxdb10 =
    (super.influxdb.overrideAttrs (oldAttrs: {

      patches = [ ./influxdb-add_increase.patch ];

    })).bin // { outputs = [ "bin" ]; };

}
