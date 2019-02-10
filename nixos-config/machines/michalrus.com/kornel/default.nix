{ config, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "a0fdcfb2509af8dc9791bd3695bfcca493012896";
    sha256 = "19n3qfl5044c8cnwwamd1i8nfqln78sbpf8fnav92irvsi9swmw6";
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
    , haskellBotNicks = [ "lambdabot" ]
    , scalaBotNicks = [ "multibot", "multibot_", "multibot1", "multibot_1", "multibot2", "multibot_2" ]
    , channels = [ "#kornel-test" ]
    , logTraffic = True
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
