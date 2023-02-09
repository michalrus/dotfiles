{ config, pkgs, lib, ... }:

{
  programs.git = {
    userEmail = "m@michalrus.com";
    userName = "Michal Rus";
    signing.key = "m@michalrus.com";
    signing.signByDefault = true;
  };

  home.file.".gnupg/sshcontrol".text = ''
    F15F9415111762A4C87B575C83B51D2861424F8C 0
  '';

  programs.gpg.settings.default-key = "m@michalrus.com";
}
