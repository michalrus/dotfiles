{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../pkgs
    ../common.nix
    ./monitoring
    ./gitolite
    ./ipsec
    ./bitlbee
    ./kornel
    ./jabber
    ./openvpn
    ./web
    ./feeds/annibot.nix
    ./feeds/stosowana.nix
    ./feeds/rss2email.nix
    ./znc
  ];

  networking.hostName = "michalrus_com";

  networking.firewall.allowedTCPPorts = [
    113  # identd
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
      permitRootLogin = pkgs.lib.mkForce "no";
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    oidentd.enable = true;
  };

  users = let

    immutableDotfiles =  map (p: "${../../../dotfiles}/${p}");
    mutableDotfiles = u: map (p: "${u.home}/.dotfiles/dotfiles/${p}");

  in {
    users.root.dotfiles  = immutableDotfiles [ "base" ];

    extraUsers.m = {
      isNormalUser = true;
      linger = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCbtBIiVRih4vlbX8ciQELf9wST0tYIygNfPxNjtm1IElpOKVT+j62hPui+d0VELKuxJcyo3tY9nf2zUYUG3PG7IWjyiHi6FyOasUQLzJrXBRj5dNsPr+SYXAyL1jsTbvbfiIUkfPAPuv5Tf/tg/lAdTriTy73V5sN7vtX+MH2k8n4agE6fhj2FAhiSwI4MAZJmIsNB2X+1GZVLZlggpN7tkkfjFWE5nCvlR+/lA6e0wl9ZCzTas112fTTBUk64wd1U7vlv1+nr7YgVAqyAQR/w7VCe0z3hrwIwxCOdW3nN19dW2gCQ7gKrZbDfaU3/OqURTNq9zwdET/mNM7unF4sX michalrus@notebook"
      ];
      description = "Michal Rus";
      extraGroups = [ "wheel" ];

      # The password is only used for sudo; should probably use custom PAM setup, cf. http://unix.stackexchange.com/a/94646
      hashedPassword = "$6$.lrNvojxVb.$rebh/ELnYtO69DyvnqL4IWE8Gsg.neIzfGTsM0NbUsl7vhblv.P.SgLQk05mJiLFMXje/9paO8DCB2M8lEfQQ1";

      dotfiles = mutableDotfiles config.users.users.m [ "base" "michalrus/base" "michalrus/personal" ];
    };
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
