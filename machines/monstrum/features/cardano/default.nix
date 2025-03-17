{ config, lib, pkgs, ... }:

let

  user = "cardano";
  uid = 2052;
  dataDir = "/var/media/${user}";

  # 10.1.4
  cardano-node-flake = builtins.getFlake "github:IntersectMBO/cardano-node/1f63dbf2ab39e0b32bf6901dc203866d3e37de08";

  # published as of 2025-02-13T10:32:15Z
  cardano-playground = builtins.getFlake "github:input-output-hk/cardano-playground/39ea4db0daa11d6334a55353f685e185765a619b";

  # 2450.0
  mithril-flake = builtins.getFlake "github:input-output-hk/mithril/c6c7ebafae0158b2c1672eb96f6ef832fd542f93";

  # `main` on 2025-03-17T14:11:50Z
  blockfrost-platform-flake = builtins.getFlake "github:blockfrost/blockfrost-platform/2e95fb41cdd0e77ab8c0f78ecb1a793ce446fe8d";

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
