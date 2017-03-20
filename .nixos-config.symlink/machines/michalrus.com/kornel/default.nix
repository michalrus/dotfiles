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
    rev = "e8e5b948ee46398a95d922ecd75aae204fde267f";
    sha256 = "0nqi5wgwcs2c85mmxv7nbkid4146a0dx21yzmpw07dh8d5v1nfxd";
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
        --channels "#stosowana"
    '';
  };

}
