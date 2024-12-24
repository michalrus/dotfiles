{ config, lib, pkgs, ... }:

let

  version = "13.2.0";

  ociImage = pkgs.dockerTools.pullImage {
    imageName = "openproject/community";
    finalImageTag = version;
    imageDigest = "sha256:a2484212b0003923fb80822fb06b030a7966f5e84d6b16e0303d7a5035ae4af8";
    sha256 = "sha256-t72oaM7FisqWENQV9QmXA9IXpCt63lUZV2cWWAAnFxY=";
  };

  # <https://gist.github.com/markasoftware/f5b2e55a2c2e3abb1f9eefcdf0bfff45>
  token = pkgs.fetchurl {
    url = "https://gist.githubusercontent.com/markasoftware/f5b2e55a2c2e3abb1f9eefcdf0bfff45/raw/148c5067e30eae04f96e3233144b4404f70ade47/enterprise_token.rb";
    hash = "sha256-FEQN89hGyI0plhhpQ4/ttaSgocs+5G8ZzXYzpAr0VWQ=";
  };

  user = "openproject";
  uid = 2040;
  dataDir = "/var/lib/${user}";
  assetsDir = "${dataDir}/assets";
  pgdataDir = "${dataDir}/pgdata";
  cidFile = "${dataDir}/${user}.cid";

  imageFullName = "${ociImage.imageName}:${ociImage.imageTag}";

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
      inherit uid;
      subUidRanges = [{ startUid = 100000; count = 65536; }];
      subGidRanges = [{ startGid = 100000; count = 65536; }];
    };
    users.groups.${user}.gid = uid;

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
        MemoryHigh = "9G";
        MemoryMax = "10G";
        Restart = "always";
        RestartSec = 10;
        Environment = "PODMAN_SYSTEMD_UNIT=${user}.service";
        Type = "notify";
        NotifyAccess = "all";
        TimeoutStartSec = 0;  # ‘podman load’ can take a long time
        TimeoutStopSec = 120;
        ExecStop = lib.getExe (pkgs.writeShellScriptBin "${user}-stop" ''
          exec podman stop --ignore --cidfile=${cidFile} --time=110
        '');
        TemporaryFileSystem = "/tmp:size=100M,mode=1777"; # otherwise it fails to start after reboot
      };
      preStart = ''
        podman image inspect ${lib.escapeShellArg imageFullName} >/dev/null || \
          podman load -i ${ociImage}
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
          -v ${token}:/app/app/models/enterprise_token.rb:ro \
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
          -e OPENPROJECT_ATTACHMENT__MAX__SIZE=65536 \
          -e OPENPROJECT_WEB_WORKERS=8 \
          -e RAILS_MIN_THREADS=16 \
          -e RAILS_MAX_THREADS=16 \
          ${lib.escapeShellArg imageFullName}
      '';
    };

    systemd.tmpfiles.rules = [
      "d ${dataDir} 0700 ${user} ${user} -"
    ];
  };
}
