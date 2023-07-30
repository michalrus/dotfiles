{ config, flake, pkgs, lib, ... }:

{

  programs.git = {
    enable = true;
    aliases = rec {
      s = "status";
      d = "diff";
      c = "diff --cached";
      a = "add";
      co = "checkout";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset %C(bold magenta)%G?%Creset%C(yellow)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --abbrev-commit --date-order";
      lga = lg + " --all";
      ff = "merge --ff-only";
    };
    extraConfig = {
      core.safecrlf = false;
      core.commentchar = ";";
      merge.conflictstyle = "diff3";
      fetch.prune = true;
      pull.ff = "only";
      transfer.fsckObjects = true;
    };
  };

  # For whatever reason this doesn’t happen on Darwin:
  home.file = if pkgs.stdenv.isDarwin then {
    ".gitconfig".text = config.xdg.configFile."git/config".text;
  } else {};

}
