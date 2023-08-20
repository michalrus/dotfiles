{ flake, config, lib, pkgs, ... }:

let

  # Don’t rebuild packages for each config.el change:
  mkDir = { withConfig ? false }: pkgs.linkFarm "my-doom-packages" [
    { name = "init.el";     path = ./init.el; }
    { name = "packages.el"; path = ./packages.el; }
    {
      name = "config.el";
      path =
        if withConfig
        then ./config.el
        else pkgs.emptyFile;
    }
  ];

in {
  imports = [ flake.inputs.nix-doom-emacs-2305.hmModule ];

  programs.doom-emacs = {
    enable = true;
    doomPackageDir = mkDir { withConfig = false; };
    doomPrivateDir = mkDir { withConfig = true; };
    extraConfig = lib.concatStringsSep "\n" [
      (if config.programs.git.userName != null then ''
        (setq user-full-name ${__toJSON config.programs.git.userName})
      '' else "")

      (if config.programs.git.userEmail != null then ''
        (setq user-mail-address ${__toJSON config.programs.git.userEmail})
      '' else "")
    ];
    emacsPackagesOverlay = self: super: {
      gumshoe = let
        version = "20230302.457";
        rev = "3b65ee2496d6de3c7c47a821b38a5a19e0b64c2a";
        hash = "sha256-NRbX95VJH0YuBHdC1fJOdp2xZy7QQKjEWI6k2I4rQvc=";
      in super.gumshoe.overrideAttrs (old: {
        inherit version;
        src = pkgs.fetchFromGitHub { owner = "Overdr0ne"; repo = "gumshoe"; inherit rev hash; };
      });
    };
  };

  services.emacs = {
    enable = true;  # emacs-daemon systemd --user service:
    client.enable = true;
    startWithUserSession = true;
  };

  systemd.user.services.emacs.Service.Restart = lib.mkForce "always";

  home.sessionVariables = if pkgs.stdenv.isLinux then rec {
    EDITOR = "emacs";
    VISUAL = EDITOR;
  } else {};

  home.shellAliases.e = "$EDITOR";

  home.packages = [
    (lib.hiPrio (pkgs.writeShellScriptBin "emacs" ''
      set -eou pipefail
      systemctl start --user emacs.service
      exec emacsclient -c "$@"
    ''))
  ];
}
