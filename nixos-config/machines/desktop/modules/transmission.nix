{ config, lib, pkgs, ... }:

let

  home = "${config.services.transmission.home}";
  shared = "${home}/shared";
  downloads = "${shared}/Downloads";
  incomplete = "${shared}/Incomplete";

in

{

  ## `services.transmission` uses the global definition… :rolling_eyes:
  nixpkgs.overlays = [
    (_: super: {
      transmission = super.transmission.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ [ ./transmission--no-deleteLocalData.patch ];
      });
    })
  ];

  services.transmission = {
    enable = true;

    settings = {
      download-dir = downloads;
      incomplete-dir = incomplete;
      incomplete-dir-enabled = true;
      rpc-authentication-required = true;
      rpc-password = "{b909b5e3c0a100d2de82b97cb3d37fd5ba6f1abbIT6yGNWp";
      speed-limit-up = 20;
      speed-limit-up-enabled = true;
    };

    downloadDirPermissions = "775";
  };

}
