{ config, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "55e790f195eee6e9d87d9d6eb7f63c9fa25e15e9";
    sha256 = "1mhgg5alqfhyljizwrj0hnqd0hsb06dv5i2hipn753ccc9sh4d31";
  };

  kornel = import src;

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
      # prevent-ifd-gc: ${src}
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
