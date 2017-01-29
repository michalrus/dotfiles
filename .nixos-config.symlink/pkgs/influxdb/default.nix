super: self:

(super.stdenv.lib.overrideDerivation super.influxdb10 (oldAttrs: {
  patches = [ ./add_increase.patch ];
})).bin // { outputs = [ "bin" ]; }
