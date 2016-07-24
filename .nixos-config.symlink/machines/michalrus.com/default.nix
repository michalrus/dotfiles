{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../pkgs
    ../../local
    ../../common.nix
    ./collectd
    ./gitolite
    ./ipsec
    ./jabber
    ./web
  ];

  nix.useChroot = true;   # use useSandbox from common.nix when in stable!

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

  nixpkgs.config.packageOverrides = super: let self = super.pkgs; in {
    # these fixes are not yet in stable…
    oidentd = super.oidentd.overrideDerivation(oldAttrs: { CFLAGS = [ "--std=gnu89" ]; });
    bindfs = super.bindfs.overrideDerivation(oldAttrs: { postFixup = ''
      ln -s $out/bin/bindfs $out/bin/mount.fuse.bindfs
    ''; });
    bitlbee = super.bitlbee.overrideDerivation(oldAttrs: rec {
      name = "bitlbee-3.4.2";
      patches = [];
      src = super.fetchurl {
        url = "mirror://bitlbee/src/${name}.tar.gz";
        sha256 = "0mza8lnfwibmklz8hdzg4f7p83hblf4h6fbf7d732kzpvra5bj39";
      };
    });
    bitlbee-facebook = super.bitlbee-facebook.overrideDerivation(oldAttrs: {
      src = super.fetchFromGitHub {
        rev = "609ca2d52d468863c99ff3539917f2049ea3df44";
        owner = "jgeboski";
        repo = "bitlbee-facebook";
        sha256 = "10wacn2hi1ly22idqcixaidf198ibj1a5h8srcd9gk7vh47c9bvm";
      };
    });
  };

  services = {
    openssh = {
      enable = true;
      permitRootLogin = pkgs.lib.mkForce "no";
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    oidentd.enable = true;

    bitlbee = {
      enable = true;
      plugins = with pkgs; [ bitlbee-facebook ];
    };
  };

  users = {
    extraUsers.m = {
      isNormalUser = true;
      openssh.authorizedKeys.keys = [
        "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCbtBIiVRih4vlbX8ciQELf9wST0tYIygNfPxNjtm1IElpOKVT+j62hPui+d0VELKuxJcyo3tY9nf2zUYUG3PG7IWjyiHi6FyOasUQLzJrXBRj5dNsPr+SYXAyL1jsTbvbfiIUkfPAPuv5Tf/tg/lAdTriTy73V5sN7vtX+MH2k8n4agE6fhj2FAhiSwI4MAZJmIsNB2X+1GZVLZlggpN7tkkfjFWE5nCvlR+/lA6e0wl9ZCzTas112fTTBUk64wd1U7vlv1+nr7YgVAqyAQR/w7VCe0z3hrwIwxCOdW3nN19dW2gCQ7gKrZbDfaU3/OqURTNq9zwdET/mNM7unF4sX michalrus@notebook"
      ];
      description = "Michal Rus";
      extraGroups = [ "wheel" ];

      # The password is only used for sudo; should probably use custom PAM setup, cf. http://unix.stackexchange.com/a/94646
      hashedPassword = "$6$.lrNvojxVb.$rebh/ELnYtO69DyvnqL4IWE8Gsg.neIzfGTsM0NbUsl7vhblv.P.SgLQk05mJiLFMXje/9paO8DCB2M8lEfQQ1";
    };
  };

  # A hack to `loginctl enable-linger m` (for multiplexer sessions to last), until this one is open: https://github.com/NixOS/nixpkgs/issues/3702
  system.activationScripts.loginctl-enable-linger-m = pkgs.lib.stringAfter [ "users" ] ''
    ${pkgs.systemd}/bin/loginctl enable-linger m
  '';

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.03";
}
