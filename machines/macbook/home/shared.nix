{
  config,
  pkgs,
  flake,
  lib,
  ...
}: {
  home.stateVersion = "22.05";

  home.packages = with pkgs; [
    anki-bin
  ];

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = builtins.path {path = flake.inputs.nix-doom-emacs-2305 + "/test/doom.d";};

    emacsPackage = pkgs.emacs.overrideAttrs (drv: {
      postInstall =
        (drv.postInstall or "")
        + (let
          doWrap = exe: let
            unwrapped = dirOf exe + "/." + baseNameOf exe + "-no-env";
          in ''
            mv $out/${exe} $out/${unwrapped}
            cat >$out/${exe} <<EOF
            #!/bin/sh
            # Wrap execution to happen inside a login Bash shell, to set all our custom Nix env.:
            exec ${pkgs.stdenv.shell} -l -c 'exec $out/${unwrapped} "\$@"' -- "\$@"
            EOF
            chmod +x $out/${exe}
          '';
        in ''
          ${doWrap "bin/emacs-${drv.version}"}
          ${doWrap "Applications/Emacs.app/Contents/MacOS/Emacs"}
        '');
    });
  };
}
