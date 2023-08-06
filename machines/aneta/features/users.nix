{ ... }:

{

  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      KbdInteractiveAuthentication = false;
    };
  };

  users = {
    extraUsers = {

      m = {
        hashedPassword = "$6$d3RxCdquTXL7.J7x$zRCbpql.Al1e1QAF0TO0ZkBpz6yQWjOh2HNbs0mwtrVm/BVvHEc31sUkZQk5d7dyc7yfZyyI61lXC1lXnqblL1";
        dotfiles-old.profiles = [ "base" "michalrus/base" ];
        openssh.authorizedKeys.keyFiles = [ ../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
        uid = 31337;
        extraGroups = [ "wheel" ];
        isNormalUser = true;
      };

      krzyszu = {
        dotfiles-old.profiles = [ "base" "michalrus/base" ];
        openssh.authorizedKeys.keys = [
          "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQDCQHunF/nYDmTCZrx3Bf7YynGW164RsaJ7WX9O1S3jV+uDKdSIJSvhVyexrSWGUHccBOBy78oxdX4Hcx9g7bT2oEu9wkKd1MyBODUSDScXgpSAxEbxqSHPhTLIbptfUlzac/mRLT2N5ilhe/SgFAutofxJZPYu5LyXMOGJ+CXqYB4p8TuseSzWHnpkE80+yUKe+TtrT2vgrRpT+vS+KJIHokielFUZp0yU2IMgl2AX+xgoiAKPI05t0O73bDmdRBeJW+wVwRWGtwCuhPuhDnLCalhLSzx7gm+nZE+ilC/LFHSo1XBLQYV+CNP7ecNRCRnqs/JgSQ55CnOOA1KdT3X9 krzysztof szudera@DESKTOP-8Q2GH6H"
        ];
        uid = 31338;
        extraGroups = [ "systemd-journal" ];
        isNormalUser = true;
      };

      root = {
        dotfiles-old.profiles = [ "base" ];
        openssh.authorizedKeys.keyFiles = [ ../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
      };

    };
  };

}
