{ ... }:

{

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.enableCompletion = true;
  };

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

}
