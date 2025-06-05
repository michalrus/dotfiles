{ config, lib, pkgs, ... }:

let

  user = "cardano";
  uid = 2052;
  dataDir = "/var/media/${user}";

  # 10.4.1
  cardano-node-flake = builtins.getFlake "github:IntersectMBO/cardano-node/420c94fbb075146c6ec7fba78c5b0482fafe72dd";

  # published as of 2025-05-26T19:49:13.000Z
  cardano-playground = builtins.getFlake "github:input-output-hk/cardano-playground/34301acf4d2229db1a881aa26aef88efbcda65f5";

  # 2450.0
  mithril-flake = builtins.getFlake "github:input-output-hk/mithril/c6c7ebafae0158b2c1672eb96f6ef832fd542f93";

  # `main` on 2025-04-07T16:07:21Z
  blockfrost-platform-flake = builtins.getFlake "github:blockfrost/blockfrost-platform/18863716fe9457abc4618f31da74383415a0acaf";

  cardano-node-configs = builtins.path {
    name = "cardano-node-configs";
    path = cardano-playground + "/static/book.play.dev.cardano.org/environments";
  };

  cardano-node = cardano-node-flake.packages.${pkgs.system}.cardano-node;
  cardano-cli = cardano-node-flake.packages.${pkgs.system}.cardano-cli;
  mithril-client = mithril-flake.packages.${pkgs.system}.mithril-client-cli;
  blockfrost-platform = blockfrost-platform-flake.packages.${pkgs.system}.default;

in

{

  environment.systemPackages = [ cardano-node cardano-cli mithril-client blockfrost-platform ];

  systemd.services.cardano-node = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
      User = user;
      Group = user;
      UMask = "0022";
      WorkingDirectory = dataDir;
      ExecStart = let
        # Make the P2P code less chatty in logs:
        configs = pkgs.runCommandNoCC "cardano-node-configs" {
          buildInputs = with pkgs; [ jq ];
        } ''
          cp -r ${cardano-node-configs} $out
          chmod -R +w $out
          find $out -name 'config.json' | while IFS= read -r configFile ; do
            jq '.
              | .TraceConnectionManager = false
              | .TracePeerSelection = false
              | .TracePeerSelectionActions = false
              | .TracePeerSelectionCounters = false
              | .TraceInboundGovernor = false
            ' "$configFile" >tmp.json
            mv tmp.json "$configFile"
          done
        '';
      in "${lib.getExe cardano-node} run"
        + " --config ${configs}/mainnet/config.json"
        + " --topology ${configs}/mainnet/topology.json"
        + " --port 55709"
        + " --socket-path ${dataDir}/node.socket"
        + " --database-path ${dataDir}/db";
    };
  };

  systemd.services.blockfrost-platform = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
      User = user;
      Group = user;
      UMask = "0022";
      WorkingDirectory = dataDir;
      ExecStart = "${lib.getExe blockfrost-platform}"
        + " --config ${config.age.secrets.blockfrost-platform-secret.path}"
        + " --server-address 0.0.0.0"
        + " --server-port 18077"
        + " --network mainnet"
        + " --log-level info"
        + " --node-socket-path ${dataDir}/node.socket"
        + " --mode compact";
    };
  };

  # We need to restart blockfrost-platform a few seconds after airvpn.service
  # for it to re-register under a new IP with IceBreakers:
  systemd.services.blockfrost-platform-delayed-restart = {
    bindsTo = [ "airvpn.service" ];
    partOf = [ "airvpn.service" ];
    after = [ "airvpn.service" ];
    wantedBy = [ "multi-user.target" ];
    serviceConfig = {
      Restart = "on-failure";
      RemainAfterExit = true;
    };
    script = ''
      set -euo pipefail
      sleep 15
      systemctl restart blockfrost-platform.service || true
    '';
  };

  age.secrets.blockfrost-platform-secret = {
    file = ../../../../secrets/blockfrost-platform-secret.age;
    owner = user;
  };

  users.users.${user} = {
    isSystemUser = true;
    group = user;
    home = dataDir;
    inherit uid;
  };
  users.groups.${user}.gid = uid;

  systemd.tmpfiles.rules = [
    "d ${dataDir} 0755 ${user} ${user} -"
  ];

}
