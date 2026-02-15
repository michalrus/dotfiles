{pkgs, ...}: {
  programs.mtr.enable = true;

  security.wrappers.fping = {
    source = "${pkgs.fping}/bin/fping";
    capabilities = "cap_net_raw+p";
    owner = "root";
    group = "root";
  };

  security.wrappers.traceroute = {
    source = "${pkgs.traceroute}/bin/traceroute";
    capabilities = "cap_net_raw+p";
    owner = "root";
    group = "root";
  };
}
