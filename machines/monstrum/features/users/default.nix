{ config, lib, pkgs, ... }:

{
  services.openssh = {
    enable = true;
    settings.PermitRootLogin = lib.mkForce "yes"; # for remote `nixos-rebuild`
    settings.PasswordAuthentication = false;
    settings.KbdInteractiveAuthentication = false;
  };

  users.users.k = {
    isNormalUser = true;
    hashedPassword = "$6$QVBZMWiZYqFo5BgJ$p3lhNfrqhfAm/AAMifWkBCNX3uB8mtDo2yUc.S13t7vuFuNihtLkcEJ/uUqT2RW/6hLcuGz4osIkFItLH8J/R0";
  };

  users.users.m = {
    isNormalUser = true;
    description = "Michal Rus";
    extraGroups = [ "wheel" ];
    hashedPassword = "$6$3OVhb4KbZYTkYGMC$F7E5yqxQyQqrxJlsVHyDx9pAbzZXYRu6QshnOeeHf7uo4KxM.8srNPvVdoCdZp5FZMkW0gFJOwCVoBtsVOFzg/";
    openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
  };

  users.users.km = {
    isNormalUser = true;
    description = "K. i Michal";
    hashedPassword = "$6$j0Bwh6III9mMWiTb$daX3N4aLF9nmYAtCWK.KRPkfskfXwqfQhBk1IWlh5H7KoQ6fZTtMUKyoK/U3d9s3pMzE5L6ZnmZo9xZ1OvmTH0";
    openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
  };

  users.users.root = {
    hashedPassword = "$6$l1BBzeSNfIvXZLO.$7kt/HJeCfbrlzvOYlJxlkaOodedBo9w1F2dMFcbwNV/.Onv3iK4mRUQmLqIG29nK82K9PcPbckBxX6LFaJ.1l1";
    openssh.authorizedKeys.keyFiles = [ ../../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
  };

  home-manager = {
    users.root.imports = [ ];
    users.k.imports = [ { home.sessionVariables.TZ = "Europe/Warsaw"; } ];
    users.m.imports = [ ];
    users.km.imports = [ { home.sessionVariables.TZ = "Europe/Warsaw"; } ];
  };
}
