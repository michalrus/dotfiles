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

  imageFullName = "${modified.buildArgs.name}:${version}";

in {
  options.services.openproject = {
    port = lib.mkOption {
      type = lib.types.port;
      default = 8055;
    };
    hostname = lib.mkOption {
      type = lib.types.str;
      default = "localhost:${config.services.openproject.port}";
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

    systemd.services.${user} = {
      path = [ config.virtualisation.podman.package ];
      wantedBy = ["multi-user.target"];
      serviceConfig = {
        User = user;
        Group = user;
        MemoryHigh = "1.4G";
        MemoryMax = "1.5G";
        Restart = "always";
        Environment = "PODMAN_SYSTEMD_UNIT=${user}.service";
        Type = "notify";
        NotifyAccess = "all";
        TimeoutStartSec = 0;  # ‘podman load’ can take a long time
        TimeoutStopSec = 120;
      };
      preStart = ''
        podman image inspect ${lib.escapeShellArg imageFullName} >/dev/null || \
          exec podman load -i ${modified}
      '';
      script = ''
        export OPENPROJECT_SECRET_KEY_BASE=$(cat ${config.age.secrets.openproject_key_base.path})
        exec podman run \
          --rm --replace --name=${user} \
          --detach --log-driver=journald \
          --cgroups=no-conmon \
          --sdnotify=conmon \
          -v ${dataDir}/assets:/var/openproject/assets \
          -v ${dataDir}/pgdata:/var/openproject/pgdata \
          -p ${toString config.services.openproject.port}:80 \
          -e OPENPROJECT_SECRET_KEY_BASE'*' \
          -e OPENPROJECT_HOST__NAME=${config.services.openproject.hostname} \
          -e OPENPROJECT_HTTPS=${if config.services.openproject.https then "true" else "false"} \
          -e OPENPROJECT_DEFAULT__LANGUAGE=en \
          -e OPENPROJECT_LOG__LEVEL=warn \
          -e RAILS_MIN_THREADS=2 \
          -e RAILS_MAX_THREADS=2 \
          ${lib.escapeShellArg imageFullName}
      '';
    };

    systemd.tmpfiles.rules = [
      "d ${dataDir}        0700 ${user} ${user} -"
      "d ${dataDir}/assets 0700 ${user} ${user} -"
      "d ${dataDir}/pgdata 0700 ${user} ${user} -"
    ];
  };
}
