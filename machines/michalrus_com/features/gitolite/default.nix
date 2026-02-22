_: {
  services.gitolite = {
    enable = true;
    adminPubkey = builtins.readFile ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub;
  };
}
