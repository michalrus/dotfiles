{
  config,
  pkgs,
  lib,
  secrets,
  ...
}: {
  programs.git = {
    userEmail = "km@monstrum";
    userName = "K i M";
    signing.key = "km@monstrum";
    signing.signByDefault = true;
  };

  programs.gpg.settings.default-key = "km@monstrum";
}
