{
  config,
  pkgs,
  lib,
  ...
}: {
  boot.tmp.useTmpfs = false;

  networking.firewall.allowedTCPPorts = [
    113 # identd
  ];

  time.timeZone = "UTC";

  environment.systemPackages = with pkgs; [
    weechat
  ];

  programs = {
    ssh.startAgent = false;
  };

  services = {
    openssh = {
      enable = true;
      settings.PermitRootLogin = lib.mkForce "yes"; # for remote `nixos-rebuild`
      settings.PasswordAuthentication = false;
      settings.KbdInteractiveAuthentication = false;
    };

    oidentd.enable = true;
    smartd.enable = false; # TODO: why won’t it work? `/dev/xvda: Unable to detect device type`

    journald.extraConfig = ''
      SystemMaxUse=200M
    '';
  };

  users = {
    users.root = {
      dotfiles-old.profiles = ["base"];
      openssh.authorizedKeys.keyFiles = [../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub];
    };

    users.m = {
      uid = 1000;
      isNormalUser = true;
      openssh.authorizedKeys.keyFiles = [../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub];
      description = "Michal Rus";
      extraGroups = ["wheel"];

      # The password is only used for sudo; should probably use custom PAM setup, cf. http://unix.stackexchange.com/a/94646
      hashedPassword = "$6$.lrNvojxVb.$rebh/ELnYtO69DyvnqL4IWE8Gsg.neIzfGTsM0NbUsl7vhblv.P.SgLQk05mJiLFMXje/9paO8DCB2M8lEfQQ1";

      #dotfiles-old.base = config.users.users.m.home + "/.dotfiles/dotfiles";
      dotfiles-old.profiles = ["base" "michalrus/base" "michalrus/personal"];
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "23.05";
}
