{
  config,
  lib,
  pkgs,
  ...
}:
with lib; let
  dataDir = "/var/lib/openvpn/nordvpn";
in {
  services.openvpn.servers.nordvpn = {
    updateResolvConf = true;
    autoStart = false;
    config = ''
      config ${dataDir}/config
      auth-user-pass ${dataDir}/auth-user-pass
    '';
  };
}
