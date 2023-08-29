{ flake, config, lib, pkgs, ... }:

let

  doom-emacs = flake.packages.${pkgs.system}.doom-emacs-mine;

in

{
  programs.emacs.enable = lib.mkForce false;  # no need for the wrapper which this option forces

  home.packages = [
    doom-emacs
    (lib.hiPrio (pkgs.writeShellScriptBin "emacs" ''
      set -eou pipefail
      systemctl start --user emacs.service
      exec emacsclient -c "$@"
    ''))
  ];

  services.emacs = {
    enable = true;  # emacs-daemon systemd --user service:
    package = doom-emacs;
    client.enable = true;
    startWithUserSession = true;
  };

  systemd.user.services.emacs.Service.Restart = lib.mkForce "always";

  home.sessionVariables = if pkgs.stdenv.isLinux then rec {
    EDITOR = "emacs";
    VISUAL = EDITOR;
  } else {};

  home.shellAliases.e = "$EDITOR";
}
