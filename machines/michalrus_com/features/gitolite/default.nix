{
  config,
  lib,
  pkgs,
  ...
}:
with lib; {
  services.gitolite = {
    enable = true;
    adminPubkey = builtins.readFile ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub;
  };
}
