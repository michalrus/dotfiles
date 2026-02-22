{pkgs, ...}: let
  user = "microsocks";
  uid = 2044;

  # XXX: keep it the same as the one in connmon.service and esp.
  # ip-route-novpn-table.service – otherwise it won’t bypass VPN
  table = 2964;
in {
  users.users.${user} = {
    isSystemUser = true;
    group = user;
    home = "/run/${user}";
    inherit uid;
  };
  users.groups.${user}.gid = uid;

  systemd.services."microsocks" = {
    wantedBy = ["multi-user.target"];
    after = ["network.target"];
    serviceConfig = {
      Type = "simple";
      Restart = "always";
      RestartSec = 5;
      RuntimeDirectory = user;
      PermissionsStartOnly = true;
      User = user;
      Group = user;
    };
    unitConfig = {
      StartLimitIntervalSec = 0; # no restart rate limiting
    };
    path = with pkgs; [microsocks iproute2];
    # Bypass VPN:
    preStart = ''
      ip rule add uidrange ${toString uid}-${toString uid} lookup ${toString table} priority 2001
    '';
    script = ''
      exec microsocks -i 0.0.0.0 -p 1080
    '';
    postStop = ''
      ip rule del uidrange ${toString uid}-${toString uid} lookup ${toString table} priority 2001 || true
    '';
  };
}
