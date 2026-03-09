{pkgs, ...}: {
  home.packages = with pkgs; [
    oath-toolkit
  ];

  programs.password-store = {
    enable = true;
    package = pkgs.pass.overrideAttrs (old: {
      # Don’t run the Conventional Commits checks on `pass` commits:
      postInstall =
        (old.postInstall or "")
        + ''
          wrapProgram $out/bin/pass --set SKIP_COG 1
        '';
      nativeBuildInputs = (old.nativeBuildInputs or []) ++ [pkgs.makeWrapper];
    });
    settings.PASSWORD_STORE_DIR = "$HOME/.password-store";
  };

  # Browser integration of password-store via Native Messaging:
  programs.browserpass.enable = true;
}
