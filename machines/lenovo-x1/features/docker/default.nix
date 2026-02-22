_: {
  virtualisation.docker.enable = true;
  users.users.m.extraGroups = ["docker"];
  users.users.mw.extraGroups = ["docker"];
}
