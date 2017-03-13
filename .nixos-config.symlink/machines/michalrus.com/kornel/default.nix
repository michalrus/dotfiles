{ config, pkgs, ... }:

let

  src = pkgs.fetchFromGitHub {
    owner = "michalrus";
    repo = "kornel";
    rev = "1d45ae8e6e18b35fcb831052cc815f353c0ae116";
    sha256 = "0bayw713xfc2j69mv0da9d7n7hy5wz4zch2pvqk47wimpw3y7wsr";
  };

  nix-src = pkgs.runCommand "kornel-nix-src" {} ''
    cp -r ${src} $out
    cd $out
    chmod +w .
    ${pkgs.cabal2nix}/bin/cabal2nix . >kornel.nix
  '';

  kornel = pkgs.unstable-haskell.packages.ghc802.callPackage "${nix-src}/kornel.nix" { };

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
        --cleverbot-api-key-file "${dataDir}"/cleverbot.key \
        --channel "#stosowana"
    '';
  };

}
