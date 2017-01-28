{ config, lib, pkgs, ... }:

with lib;

let

  port = 6698;

  clientbufferFixed = overrideDerivation pkgs.zncModules.clientbuffer (oldAttrs: {
    src = pkgs.fetchFromGitHub {
      owner = "jpnurmi";
      repo = "znc-clientbuffer";
      rev = "af4cdff9fda5683a923744b065e53af34eb71d76";
      sha256 = "18035c0iyxypx43b302b4xchavamgr6kr9jkvw90l97i2hqyvdf4";
    };
    patches = [
      ./clientbuffer-self-message.patch
      ./clientbuffer-high-io.patch
    ];
  });

  acmeCert = "michalrus.com";

  zncDerivation = a@{
    src, module_name, version,
    buildPhase ? "${pkgs.znc}/bin/znc-buildmod ${module_name}.cpp",
    installPhase ? "install -D ${module_name}.so $out/lib/znc/${module_name}.so", ...
  } : pkgs.stdenv.mkDerivation (a // {
    inherit buildPhase;
    inherit installPhase;
    name = "znc-${module_name}-${version}";
    passthru.module_name = module_name;
  });

  antiidle = zncDerivation rec {
    version = "unknown";
    module_name = "antiidle";
    src = pkgs.fetchurl {
      url = "https://raw.githubusercontent.com/znc/znc/34d0da2097f860af022c16113cc37e8ebf85da83/modules/antiidle.cpp";
      sha256 = "0lqrrqkb2zq7y5znj83ajsxsj7pclim57pnrngcj8r289jvg5541";
    };
    unpackPhase = "cp $src antiidle.cpp";
  };

  autoinvitejoin = zncDerivation rec {
    version = "20160621";
    module_name = "autoinvitejoin";
    src = pkgs.fetchFromGitHub {
      owner = "Cizzle";
      repo = "znc-autoinvitejoin";
      rev = "d0b61291c5ac7e85c049836f96ce88e141251b1f";
      sha256 = "1388dd2ncll42xxckrjyajwbfxwbgq0qgx65vpa27q62ms98jzkd";
    };
    patches = [ ./autoinvitejoin-all.patch ];
  };

  cfg = config.services.znc;

  # Prepare the server ACME cert for ZNC.
  createZncPem = let
    src = "${config.security.acme.directory}/${acmeCert}";
    dh  = "${cfg.dataDir}/dhparam.pem";
    dst = "${cfg.dataDir}/znc.pem";
  in pkgs.writeScript "create-znc-pem" ''
    [ -e "${dh}" ] || ${pkgs.openssl.bin}/bin/openssl dhparam -out "${dh}" 4096
    (
      cat "${src}/key.pem" ; echo # why no \n at EOL?
      cat "${src}/fullchain.pem" ; echo
      cat "${dh}"
    ) >"${dst}"
    chmod 444 "${dst}" # safe, its directory is 700 znc:znc
  '';

in

{
  networking.firewall.allowedTCPPorts = [ port ];

  # Unfortunately, the znc module doesn’t have the package configurable.
  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    znc = super.znc.overrideDerivation (oldAttrs: {
      patches = [
        ./timeouts.patch
        ./always-has-self-message.patch
        ./log-timestamp.patch
        ./log-join-part-quit.patch
      ];
    });
  };

  services = {
    znc = {
      enable = true;
      modulePackages = [ clientbufferFixed antiidle autoinvitejoin ] ++
        (with pkgs.zncModules; [ playback push ]);
      mutable = true;
      zncConf = ''
        # Generate the config with `znc -c`.
        # It’s not here, because it contains sensitive data.
        # (And it’s pretty convenient for it to be mutable.)
      '';
    };
  };

  # Fix missing `exec`.
  systemd.services.znc.script = mkForce "exec ${pkgs.znc}/bin/znc --foreground --datadir ${cfg.dataDir} ${toString cfg.extraFlags}";

  # Note that there’s no need to restart znc.service itself, when ACME
  # updates the cert, as `znc.pem` is re-read every time a new client
  # connects.
  security.acme.certs."${acmeCert}".postRun = "${createZncPem}";
  systemd.services.znc.serviceConfig.PermissionsStartOnly = true;
  systemd.services.znc.preStart = ''
    ${createZncPem}
    chown -R "${cfg.user}:${cfg.user}" "${cfg.dataDir}" # group == cfg.user? smelly…
  '';

}
