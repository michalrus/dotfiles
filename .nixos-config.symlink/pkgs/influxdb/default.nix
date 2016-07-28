super: self:

(super.stdenv.lib.overrideDerivation super.influxdb (oldAttrs: {
  patches = [ ./add_increase.patch ];
})).bin // { outputs = [ "bin" ]; }
