{
  config,
  pkgs,
  lib,
  secrets,
  ...
}: {
  programs.git = {
    settings.user.email = "km@monstrum";
    settings.user.name = "K i M";
    signing.key = "km@monstrum";
    signing.signByDefault = true;
  };

  programs.gpg.settings.default-key = "km@monstrum";
}
