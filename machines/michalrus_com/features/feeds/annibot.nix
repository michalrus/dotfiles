{
  config,
  pkgs,
  ...
}: let
  dataDir = "/var/lib/annibot";
  user = "annibot";
  group = user;
  runAt = "2:00";

  annibot = pkgs.stdenv.mkDerivation {
    name = "annibot";
    src = pkgs.fetchFromGitHub {
      rev = "69c07ed842c33be4edc094bc043cdebe999b6472";
      owner = "michalrus";
      repo = "annibot";
      sha256 = "1vb1m5a9xdxcnx05f9s0a3d46n7yz64wiqql37wl4zf0xq1zxw5i";
    };
    buildInputs = with pkgs;
      [makeWrapper perl]
      ++ (with perlPackages; [
        NetSMTPSSL
        XMLSimple
        IPCRun3
        DateCalc
        MIMEBase64
        AuthenSASL
      ]);
    installPhase = ''
      mkdir -p $out/libexec $out/sample
      cp get-all login run $out/libexec
      cp config.cfg.sample $out/sample
      substituteInPlace $out/libexec/login \
        --replace '$(dirname "$(readlink -f "$0")")' '${dataDir}'
      substituteInPlace $out/libexec/get-all \
        --replace '$(dirname "$(readlink -f "$0")")' '${dataDir}' \
        --replace "\''${dir}/login" "$out/libexec/login"
      substituteInPlace $out/libexec/run \
        --replace "dirname(\$0) . '/get-all'" "\"$out/libexec/get-all\"" \
        --replace 'dirname($0)' '"${dataDir}"'
      wrapProgram $out/libexec/run --prefix PERL5LIB : "$PERL5LIB"
    '';
  };
in {
  age.secrets.annibot_smtp = {
    file = ../../../../secrets/smtp_scripts_michalrus_com.age;
    owner = user;
  };

  systemd.services.annibot = {
    description = "Check contacts’ birthdays and notify about upcoming by mail.";
    serviceConfig = {
      User = user;
      Group = group;
      PermissionsStartOnly = true;
    };
    path = with pkgs; [bc curl];
    preStart = ''
      mkdir -p "${dataDir}"

      cat >"${dataDir}"/config.cfg <<EOF
      MailTo           "m@michalrus.com"
      GmailFromName    "annibot"
      GmailFromEmail   "scripts@michalrus.com"
      GmailPassword    "$(cat ${config.age.secrets.annibot_smtp.path})"
      EOF

      chown -R "${user}:${group}" "${dataDir}"
      chmod -R u=rwX,g=,o= "${dataDir}"
    '';
    script = "exec ${annibot}/libexec/run";
  };

  systemd.timers.annibot = {
    partOf = ["annibot.service"];
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
