{ flake, config, lib, pkgs, ... }:

let

  doom-emacs = let
    underlying = flake.packages.${pkgs.system}.doom-emacs-mine;
  in if !pkgs.stdenv.isDarwin then underlying else underlying.overrideAttrs (old: {
    # On macOS, I want the icon to launch `emacsclient` for the daemon we run below:
    buildCommand = old.buildCommand + ''
      chmod -R +w $out
      launcher=$out/Applications/Emacs.app/Contents/MacOS/Emacs
      rm -f $launcher
      cat >$launcher <<EOF
      #!/bin/sh
      exec $out/bin/emacsclient -c "\$@"
      EOF
      chmod +x $launcher
    '';
  });

in

{
  # No need for the wrapper which this option forces:
  programs.emacs.enable = lib.mkForce false;

  home.packages = [
    doom-emacs
    (lib.hiPrio (pkgs.writeShellScriptBin "emacs" ''
      set -eou pipefail
      ${if pkgs.stdenv.isLinux then ''
        systemctl start --user emacs.service
      '' else ""}
      exec ${doom-emacs}/bin/emacsclient -c "$@"
    ''))
  ];

  services = if pkgs.stdenv.isLinux then {
    emacs = {
      enable = true;  # emacs-daemon systemd --user service:
      package = doom-emacs;
      client.enable = true;
      startWithUserSession = true;
    };
  } else {};

  # XXX: services.emacs doesn't support Darwin yet, so let's define our own:
  launchd.agents = if pkgs.stdenv.isDarwin then {
    emacs = {
      enable = true;
      config = {
        Label = "org.emacs.daemon";
        ProgramArguments = [(lib.getExe (pkgs.writeShellScriptBin "emacs-daemon" ''
          set -eou pipefail
          # Load the Nix env, by using a login shell:
          exec ${pkgs.stdenv.shell} -l -c 'exec ${doom-emacs}/bin/emacs --fg-daemon'
        ''))];
        RunAtLoad = true;
        KeepAlive = true;
      };
    };
  } else {};

  systemd.user.services.emacs.Service.Restart = lib.mkForce "always";

  home.sessionVariables = if pkgs.stdenv.isLinux then rec {
    EDITOR = "emacs";
    VISUAL = EDITOR;
  } else {};

  home.shellAliases.e = "$EDITOR";
}
