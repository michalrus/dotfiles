{pkgs, ...}: let
  kornel = let
    raw = pkgs.fetchFromGitHub {
      owner = "michalrus";
      repo = "kornel";
      rev = "3aa5bfca8f44b13be1404b333dc0f05dd41483e8";
      sha256 = "06msh22z3hjqhlyan3kif3yqdhz43bd0bhvi7d5j55z9j10h5jny";
    };
    patched = pkgs.runCommand "kornel-pure-src" {} ''
      cp -r ${raw} $out
      chmod -R +w $out
      cd $out
      patch -p1 -i ${./pure-build.patch}
    '';
  in
    import patched;

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
in {
  users.extraUsers."${user}" = {
    group = user;
    home = dataDir;
    isSystemUser = true;
  };
  users.extraGroups."${user}" = {};

  systemd.services.kornel = {
    after = ["network.target"];
    wantedBy = ["multi-user.target"];
    serviceConfig = {
      User = user;
      Group = user;
      PermissionsStartOnly = true;
    };
    path = [kornel];
    preStart = ''
      mkdir -p "${dataDir}"
      chown -R "${user}:${user}" "${dataDir}"
      chmod 750 "${dataDir}"
    '';
    script = ''exec kornel-exe -c ${configFile}'';
  };
}
