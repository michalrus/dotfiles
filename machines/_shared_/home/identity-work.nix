{
  config,
  pkgs,
  lib,
  ...
}: {
  programs.git = {
    userEmail = "michal.rus@iohk.io";
    userName = "Michal Rus";
    signing.key = "michal.rus@iohk.io";
    signing.signByDefault = true;
  };

  home.file.".gnupg/sshcontrol".text = ''
    B05520F6A1ECEE40238CEC10DE835DEA6D5089A1
  '';

  programs.gpg.settings.default-key = "michal.rus@iohk.io";

  home.file.".avatar.jpg".source = ../assets/avatar.jpg;
  home.file.".wallpaper.png".source = ../assets/wallpapers/yosemite.png;
}
