{ config, lib, pkgs, ... }:

with lib;

# If ConnMan is enabled, importing this will merge definitions in
# networking.wireless.networks into one of ConnMan’s config files. A
# bit more declarative?

let

  dir = "/var/lib/connman";

  svcs = concatStrings(mapAttrsToList (name: value: ''
    [service_${replaceStrings [" "] ["_"] name}]
    Type = wifi
    Name = ${name}
    Passphrase = ${value.psk}

  '') config.networking.wireless.networks);

in

mkIf config.networking.connman.enable {
  systemd.services.connman.preStart = ''
    mkdir -p ${dir}
    ln -fs ${builtins.toFile "connman-services" svcs} ${dir}/nixos.config
  '';
}
