{
  config,
  pkgs,
  ...
}: let
  # Note, after enabling this, you have to:
  #   sudo -u rss2email bash
  #   cd ~
  # … and setup the feeds manually using `r2e`.
  user = "rss2email";
  dataDir = "/var/lib/rss2email";
  runAt = "*:0/20"; # every 20 minutes

  python = pkgs.python3Packages;

  old-html2text = python.buildPythonPackage {
    name = "html2text";
    src = pkgs.fetchFromGitHub {
      owner = "Alir3z4";
      repo = "html2text";
      rev = "d9bf7d693ea2589bce9f4cfba945e13698d3ad70";
      sha256 = "0z8676xkbka5y86l40m2n2xkx4rlsiwinh8894hcaf8bbpglxgmq";
    };
  };

  rss2email = python.buildPythonApplication {
    name = "rss2email";
    src = pkgs.fetchFromGitHub {
      owner = "wking";
      repo = "rss2email";
      rev = "0489b589160306794f21341f767118abd91f4ece";
      sha256 = "02a4py7zx9ib49kijpjckya1pb8xlb3gmp7s1dybsdjmwqbdyzx7";
    };
    buildInputs = [pkgs.makeWrapper];
    propagatedBuildInputs = [python.feedparser old-html2text python.beautifulsoup4];
  };
in {
  users.extraUsers."${user}" = {
    group = user;
    home = dataDir;
    isSystemUser = true;
  };
  users.extraGroups."${user}" = {};

  systemd.services.rss2email = {
    serviceConfig = {
      User = user;
      Group = user;
      PermissionsStartOnly = true;
    };
    environment = {
      XDG_DATA_HOME = config.users.extraUsers."${user}".home;
      XDG_CONFIG_HOME = config.users.extraUsers."${user}".home;
    };
    path = [rss2email];
    preStart = ''
      mkdir -p "${dataDir}"
      ln -sf "${rss2email}/bin/r2e" "${dataDir}/r2e"
      chown -R "${user}:${user}" "${dataDir}"
      chmod 750 "${dataDir}"
    '';
    script = ''
      exec r2e run
    '';
  };

  systemd.timers.rss2email = {
    partOf = ["rss2email.service"];
    wantedBy = ["timers.target"];
    timerConfig.OnCalendar = runAt;
  };
}
