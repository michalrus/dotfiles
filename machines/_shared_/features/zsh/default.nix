_: {
  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.completion.enable = true;
  };

  users.defaultUserShell = "/run/current-system/sw/bin/zsh";

  # Don’t escape '#' when auto-completing for Nix flakes:
  nixpkgs.overlays = [
    (_: super: {
      zsh = super.zsh.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ [./zsh-no-escape-hash.patch];
      });
    })
  ];
}
