{ config, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "3aa5bfca8f44b13be1404b333dc0f05dd41483e8";
    sha256 = "06msh22z3hjqhlyan3kif3yqdhz43bd0bhvi7d5j55z9j10h5jny";
  };

  kornel = import src;

  user = "kornel";
  dataDir = "/var/lib/${user}";

  configFile = pkgs.writeText "kornel.dhall" ''
    { serverHost = "irc.freenode.com"
    , serverPort = 6697
    , usingSSL = True
    , nick = "kornel"
    , saslPassword = [ ${dataDir}/nickserv.pass as Text ] : Optional Text
    , nickservPassword = [] : Optional Text
    , httpSnippetsFetchMax = Natural/toInteger (100 * 1024)
    , cleverBotApiKey = [ ${dataDir}/cleverbot.key as Text ] : Optional Text
    , smmryApiKey = [ ${dataDir}/smmry.key as Text ] : Optional Text
    , wolframApiKey = [ ${dataDir}/wolfram.key as Text ] : Optional Text
    , haskellBotNicks = [ "lambdabot" ]
    , scalaBotNicks = [ "multibot", "multibot_", "multibot1", "multibot_1", "multibot2", "multibot_2" ]
    , channels = [ "#scala.pl", "#stosowana" ]
    , logTraffic = False
    }
  '';

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
    script = ''exec kornel-exe -c ${configFile}'';
  };

}
