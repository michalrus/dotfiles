{ config, pkgs, ... }:

let

  kornel = import (pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "100d20772667e097deed80ab9bc46e5aa06251ed";
    sha256 = "13va2c4iyr5a3zq19glp8gqm7p4mppqdkp484n7zdadpizqbjaxh";
  });

  user = "kornel";
  dataDir = "/var/lib/${user}";

in

{

  users.extraUsers  = [ { name = user; group = user; home = dataDir; } ];
  users.extraGroups = [ { name = user; } ];

  systemd.services.kornel = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      User = user;
      Group = user;
      PermissionsStartOnly = true;
    };
    path = with pkgs; [ kornel ];
    preStart = ''
      mkdir -p "${dataDir}"
      chown -R "${user}:${user}" "${dataDir}"
      chmod 750 "${dataDir}"
    '';
    script = ''
      exec kornel-exe \
        --host "irc.freenode.com" --port "6697" --ssl \
        --nick "kornel" --nickserv-password-file "${dataDir}"/nickserv.pass \
        --http-snippets-fetch-max "$((100 * 1024))" \
        --cleverbot-api-key-file "${dataDir}"/cleverbot.key \
        --haskell-bot-nicks "lambdabot" \
        --scala-bot-nicks "multibot,multibot_,multibot_1,multibot_2" \
        --channels "#stosowana"
    '';
  };

}
