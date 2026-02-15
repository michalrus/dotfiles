{
  config,
  pkgs,
  ...
}: let
  dataDir = "/var/lib/stosowana";
  user = "stosowana";
  group = user;
  runAt = "*:0/15"; # every 15 minutes

  CryptRandomSeed = pkgs.perlPackages.buildPerlPackage {
    pname = "Crypt-Random-Seed";
    version = "0.03";
    src = pkgs.fetchurl {
      url = mirror://cpan/authors/id/D/DA/DANAJ/Crypt-Random-Seed-0.03.tar.gz;
      sha256 = "1wfhr2jj5mq1z1m75c1kznk8irn8q54lw3ncpckcq29ca95sagar";
    };
  };

  BytesRandomSecure = pkgs.perlPackages.buildPerlPackage {
    pname = "Bytes-Random-Secure";
    version = "0.29";
    src = pkgs.fetchurl {
      url = mirror://cpan/authors/id/D/DA/DAVIDO/Bytes-Random-Secure-0.29.tar.gz;
      sha256 = "1bl5mvcx8ggwgv5s2jg85fxni2hqgif636k1gjhgq7m1wqwx7fsk";
    };
    propagatedBuildInputs = with pkgs.perlPackages; [MathRandomISAAC CryptRandomSeed];
  };

  phpbb3mail = pkgs.stdenv.mkDerivation {
    name = "phpbb3mail";
    src = pkgs.fetchFromGitHub {
      rev = "36e41629781ed8d73a972f1823615d1edecb0de5";
      owner = "michalrus";
      repo = "phpbb3mail";
      sha256 = "09wivd6833m3f68bqlm88mrk8ap2kf4x7icbqwph7gmzwkm40par";
    };
    buildInputs = with pkgs;
      [makeWrapper perl]
      ++ (with perlPackages; [
        DBI
        DBDSQLite
        NetSMTPSSL
        AuthenSASL
        BytesRandomSecure
        LWPUserAgent
        LWPProtocolHttps
        DateCalc
      ]);
    installPhase = ''
      mkdir -p $out/libexec $out/sample
      cp run $out/libexec
      cp config.cfg.sample $out/sample
      substituteInPlace $out/libexec/run \
        --replace 'dirname($0)' '"${dataDir}"'
      wrapProgram $out/libexec/run --prefix PERL5LIB : "$PERL5LIB"
    '';
  };
in {
  systemd.services.stosowana = {
    description = "Check new posts on Stosowana.pl and notify by mail.";
    serviceConfig = {
      User = user;
      Group = group;
      PermissionsStartOnly = true;
    };
    preStart = ''
      mkdir -p "${dataDir}"
      ln -sf ${phpbb3mail}/sample/* "${dataDir}"
      chown -R "${user}:${group}" "${dataDir}"
      chmod 750 "${dataDir}"
    '';
    script = "exec ${phpbb3mail}/libexec/run";
  };

  systemd.timers.stosowana = {
    partOf = ["stosowana.service"];
    wantedBy = ["timers.target"];
    timerConfig.OnCalendar = runAt;
  };

  users = {
    extraUsers."${user}" = {
      isSystemUser = true;
      group = user;
    };
    extraGroups."${group}" = {};
  };
}
