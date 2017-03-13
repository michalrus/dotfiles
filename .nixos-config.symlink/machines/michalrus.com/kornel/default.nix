{ config, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "60b1c643ba5eb85f9c98e20973f16bcb4eee7505";
    sha256 = "0xjsfd9rnrjxax5nm3v5jxz0jd5psdg0jcgnwwkvyfvqg52in2vs";
  };

  nix-src = pkgs.runCommand "kornel-nix-src" {} ''
    cp -r ${src} $out
    cd $out
    chmod +w .
    ${pkgs.cabal2nix}/bin/cabal2nix . >kornel.nix
  '';

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

  kornel = compiler.callPackage "${nix-src}/kornel.nix" { };

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
        --host irc.freenode.com --port 6697 --ssl \
        --nick kornel --nickserv-password-file "${dataDir}"/nickserv.pass \
        --http-snippets-fetch-max $((100 * 1024)) \
        --cleverbot-api-key-file "${dataDir}"/cleverbot.key \
        --channel "#stosowana"
    '';
  };

}
