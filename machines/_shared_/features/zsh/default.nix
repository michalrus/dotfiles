_: {
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.completion.enable = true;
  };

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";
}
