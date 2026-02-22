{pkgs, ...}: {
  home.packages = with pkgs; [
    oath-toolkit
  ];

  programs.password-store = {
    enable = true;
    package = pkgs.pass;
    settings.PASSWORD_STORE_DIR = "$HOME/.password-store";
  };

  # Browser integration of password-store via Native Messaging:
  programs.browserpass.enable = true;
}
