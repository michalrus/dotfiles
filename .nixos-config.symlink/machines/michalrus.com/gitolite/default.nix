{ config, lib, pkgs, ... }:

with lib;

{
  services.gitolite = {
    enable = true;
    adminPubkey = builtins.head config.users.extraUsers.m.openssh.authorizedKeys.keys;
  };
}
