{ config, flake, pkgs, lib, ... }:

{

  home.sessionVariables = if pkgs.stdenv.isLinux then rec {
    EDITOR = "emacs";
    VISUAL = EDITOR;
  } else {};

}
