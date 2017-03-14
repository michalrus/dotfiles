{ config, pkgs, ... }:

let

  compiler = pkgs.unstable-haskell.packages.ghc802.override {
    overrides = self: super: {
      "html-entities" = pkgs.lib.overrideDerivation super.html-entities (oldAttrs: {
        postPatch = ''
          substituteInPlace html-entities.cabal \
            --replace 'directory == 1.2.*' 'directory == 1.3.*'
        '';
      });
    };
  };

  kornel = compiler.callCabal2nix "kornel" (pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "277fe37e88ef843ca652de69ec7dc75ce4806999";
    sha256 = "1xwq2kcxgyqisdbaa6kry41kz030dbq1m9qajf8b7v6jy8m3gk2w";
  }) {};

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
        --scala-bot-nicks "multibot_,multibot_1,multibot_2" \
        --channel "#stosowana"
    '';
  };

}
