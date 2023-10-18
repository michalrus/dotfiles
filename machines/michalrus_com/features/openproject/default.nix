{ config, lib, pkgs, ... }:

let

  version = "13.0.4";

  base = pkgs.dockerTools.pullImage {
    imageName = "openproject/community";
    finalImageTag = version;
    imageDigest = "sha256:1f80503b4f220d99d1cae7ac7418d81662c374b211942d0cca9f84418c4d47f1";
    sha256 = "sha256-pKA0CHKaT4f6UUl1TYZeHxiFe4AKyV2CSlams+xMB28=";
  };

  imageConfig = fromImage: builtins.fromJSON (builtins.unsafeDiscardStringContext (builtins.readFile (
    pkgs.runCommandNoCC "image-config" {
      nativeBuildInputs = [ pkgs.jq ];
    } ''
      tar -xf ${fromImage} manifest.json
      configJson=$(jq -r '.[0].Config' manifest.json)
      tar -xf ${fromImage} "$configJson"
      jq .config <"$configJson" >$out
    ''
  )));

  # <https://gist.github.com/markasoftware/f5b2e55a2c2e3abb1f9eefcdf0bfff45>
  token = pkgs.fetchurl {
    url = "https://gist.githubusercontent.com/markasoftware/f5b2e55a2c2e3abb1f9eefcdf0bfff45/raw/148c5067e30eae04f96e3233144b4404f70ade47/enterprise_token.rb";
    hash = "sha256-FEQN89hGyI0plhhpQ4/ttaSgocs+5G8ZzXYzpAr0VWQ=";
  };

  modified = pkgs.dockerTools.buildImage {
    name = "openproject/modified";
    tag = version;
    fromImage = base;
    config = imageConfig base;
    runAsRoot = ''
      #!${pkgs.runtimeShell}
      cp ${token} /app/app/models/enterprise_token.rb
    '';
    diskSize = 3072;
  };

  user = "openproject";
  dataDir = "/var/lib/${user}";
  assetsDir = "${dataDir}/assets";
  pgdataDir = "${dataDir}/pgdata";
  cidFile = "${dataDir}/${user}.cid";

  imageFullName = "${modified.buildArgs.name}:${version}";

in {
  options.services.openproject = {
    port = lib.mkOption {
      type = lib.types.port;
      default = 8055;
    };
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "localhost:${toString config.services.openproject.port}";
    };
    https = lib.mkOption {
      type = lib.types.bool;
      default = false;
    };
  };

  config = {
    # For ‘/etc/containers/policy.json’, otherwise ‘podman load’ doesn’t work:
    virtualisation.podman.enable = true;

    users.users.${user} = {
      isSystemUser = true;
      group = user;
      home = dataDir;
      autoSubUidGidRange = true;  # for rootless podman
    };
    users.groups.${user} = {};

    age.secrets.openproject_key_base = {
      file = ../../../../secrets/openproject_key_base.age;
      owner = user;
    };

    age.secrets.openproject_smtp = {
      file = ../../../../secrets/smtp_scripts_michalrus_com.age;
      owner = user;
    };

    systemd.services.${user} = {
      path = [ config.virtualisation.podman.package ];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        User = user;
        Group = user;
        MemoryHigh = "1.6G";
        MemoryMax = "1.7G";
        Restart = "always";
        Environment = "PODMAN_SYSTEMD_UNIT=${user}.service";
        Type = "notify";
        NotifyAccess = "all";
        TimeoutStartSec = 0;  # ‘podman load’ can take a long time
        TimeoutStopSec = 120;
        ExecStop = lib.getExe (pkgs.writeShellScriptBin "${user}-stop" ''
          exec podman stop --ignore --cidfile=${cidFile} --time=110
        '');
      };
      preStart = ''
        podman image inspect ${lib.escapeShellArg imageFullName} >/dev/null || \
          exec podman load -i ${modified}
        [ -e ${assetsDir} ] || { mkdir -p ${assetsDir} && chown ${user}:${user} ${assetsDir} ; }
        [ -e ${pgdataDir} ] || { mkdir -p ${pgdataDir} && chown ${user}:${user} ${pgdataDir} ; }
      '';
      # All possible settings: https://github.com/opf/openproject/blob/v13.0.4/docs/installation-and-operations/configuration/environment/README.md?plain=1#L114
      script = ''
        export OPENPROJECT_SECRET_KEY_BASE=$(cat ${config.age.secrets.openproject_key_base.path})
        export OPENPROJECT_SMTP__PASSWORD=$(cat ${config.age.secrets.openproject_smtp.path})
        exec podman run \
          --rm --replace --name=${user} \
          --detach --log-driver=journald \
          --cidfile=${cidFile} \
          --cgroups=no-conmon \
          --sdnotify=conmon \
          -v ${assetsDir}:/var/openproject/assets \
          -v ${pgdataDir}:/var/openproject/pgdata \
          -p ${toString config.services.openproject.port}:80 \
          -e OPENPROJECT_SECRET_KEY_BASE'*' \
          -e OPENPROJECT_HOST__NAME=${config.services.openproject.hostname} \
          -e OPENPROJECT_HTTPS=${if config.services.openproject.https then "true" else "false"} \
          -e OPENPROJECT_DEFAULT__LANGUAGE=en \
          -e OPENPROJECT_AUTOLOGIN=7 \
          -e OPENPROJECT_SELF__REGISTRATION=0 \
          -e OPENPROJECT_LOG__LEVEL=warn \
          -e OPENPROJECT_LOGIN__REQUIRED=true \
          -e OPENPROJECT_MAIL__FROM='OpenProject <scripts@michalrus.com>' \
          -e OPENPROJECT_EMAIL__DELIVERY__METHOD=smtp \
          -e OPENPROJECT_SMTP__ADDRESS=smtp.gmail.com \
          -e OPENPROJECT_SMTP__PORT=465 \
          -e OPENPROJECT_SMTP__DOMAIN=michalrus.com \
          -e OPENPROJECT_SMTP__AUTHENTICATION=plain \
          -e OPENPROJECT_SMTP__USER__NAME='scripts@michalrus.com' \
          -e OPENPROJECT_SMTP__PASSWORD'*' \
          -e OPENPROJECT_SMTP__SSL=true \
          -e OPENPROJECT_SMTP__OPENSSL__VERIFY__MODE=peer \
          -e OPENPROJECT_SMTP__ENABLE__STARTTLS__AUTO=false \
          -e OPENPROJECT_BCC__RECIPIENTS=true \
          -e OPENPROJECT_WELCOME__ON__HOMESCREEN=false \
          -e RAILS_MIN_THREADS=2 \
          -e RAILS_MAX_THREADS=2 \
          ${lib.escapeShellArg imageFullName}
      '';
    };

    systemd.tmpfiles.rules = [
      "d ${dataDir} 0700 ${user} ${user} -"
    ];
  };
}
