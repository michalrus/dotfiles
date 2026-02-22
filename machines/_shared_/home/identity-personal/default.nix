{secrets, ...}: {
  programs.git = {
    settings.user.email = "m@michalrus.com";
    settings.user.name = "Michal Rus";
    signing.key = "m@michalrus.com";
    signing.signByDefault = true;
  };

  programs.gpg.settings.default-key = "m@michalrus.com";

  home = {
    file = {
      ".gnupg/sshcontrol".text = ''
        F15F9415111762A4C87B575C83B51D2861424F8C 0
      '';

      ".avatar.jpg".source = ../../assets/avatar.jpg;
      ".wallpaper.png".source = ../../assets/wallpapers/yosemite.png;

      ".ssh/config.d/michalrus.com".text = ''
        Host m
          Hostname michalrus.com
          HostKeyAlias michalrus.com
          UserKnownHostsFile ~/.ssh/known_hosts.d/michalrus.com
          User m

        Host annex
          Hostname michalrus.com
          HostKeyAlias michalrus.com
          UserKnownHostsFile ~/.ssh/known_hosts.d/michalrus.com
          User gitolite
          IdentitiesOnly yes
          IdentityFile ${secrets.ssh-key-personal-git-annex.path}
      '';

      ".ssh/known_hosts.d/michalrus.com".text = ''
        michalrus.com ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHAq3I/JNJoWzLYn6/KSWiG3IfFthdeMGuWpRm0OSM4I
        michalrus.com ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDF3c2xA27VyYb3wgZaQ4NRkmTAwdOLRvUWOf/Bk6tfuH6jZAclp1uZ7ePBh9eeQxyEO668eKxphGRxQiWn34z0A9oUyfD6BWiIo8aS3t3/A1RRvhQfC+W7o00FoEkq0riBM4QoqvyGrnBt0xBnAMVAMBYdm2OPmYc6EyiDDg81WexonIvNXwZrbbyemjFh+v32j8lZ1b9gkqPgQ1qFX8hPpN+bUYU7voOmdAlpKw7tzRQqdYlml/tbKHuSbhTgCmam4btHf1VjtxGf8CWIkcUHVt3CSolGwJjP0bOmVuyy3uoZ5k4dn3SNxKcnmFkAnwJg6i9Mtvr6Ry9dYoOZ24ebk/o/scGVelBK2GbfmSrTOK/24lWKUgJScvKBZne8h+iCkS1eVe9lEk54TFEsqET1us5zk7PoaHshdWAOfnlDHza98oZPnEWXAuaiKijbBo9MWLLpsCS1sk5/RSi9d0MFagyPth/IcdBcRnKhmK7P2/lPFwpTiNVtfciyuw/LaaLU7wFkgaDcic8hYrVmJCXuG1LLRR1XvZw+OElttk+HmB6gQd3bUtas6WJm0mo0QDDz2hHlipUufRA5f7rh1MuaPQ5DXgC6xRxA1mRmZ7Hzjcjc45ju8oOTWFgnmfAoWamYupCaxXyaVdlzU2LboCCVKHEYxkIKwp3Xp/8P6tPH/Q==
      '';

      ".ssh/config.d/home-network".text = ''
        Host monstrum 10.77.2.1
          Hostname 10.77.2.1
          HostKeyAlias monstrum
          UserKnownHostsFile ~/.ssh/known_hosts.d/home-network
          User root

        Host macbook 10.77.2.99
          Hostname 10.77.2.99
          HostKeyAlias macbook
          UserKnownHostsFile ~/.ssh/known_hosts.d/home-network
          User m

        Host dell-home-server 10.77.2.11
          Hostname 10.77.2.11
          HostKeyAlias dell-home-server
          UserKnownHostsFile ~/.ssh/known_hosts.d/home-network
          User root

        Host aneta 10.77.2.13
          Hostname 10.77.2.13
          HostKeyAlias aneta
          UserKnownHostsFile ~/.ssh/known_hosts.d/home-network
          User root
      '';

      ".ssh/known_hosts.d/home-network".text = ''
        aneta ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIB1nWQ6Rb+a8oxWf5IgfzSAENh7Kug0QuNRpaZM/47Qf
        aneta ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQDoM/52op8WoSO26i2SJEpZVMunHpbCDu/rEMOQ3BW9xBvtQIWb8ViveUsuEhp1e0xYCj5DuMAPixJZPGFQRjEuOKcStyvHW117UsKcuCcsk2qqaEIgt398d31fBQJwBWfAY46wuYkMp2TUUfpkyPRMHjIsRrZrcz5sv0Kh/YmVyiZ9mUOtJx9GBA09uXyDwB0Rh7RK3W3G7CvPnRzPXKGfNzrW06uofpfQF633rxRKi0DpxINTQ8Ft8C4/jg2lt79Ln3D+sxmvKjb8kIKIaLCl7l81OTy/yrVoo1Cv/c39nTjB+A7lzGRWCj671NIUzKXxjsGiCa8HKAbIQ5T/o9aMDQpjtoDHyG5AY0i4VEzsykkF3DawzNXW7p0mZnLMSuiJx5TqvpaJmVcuYi+OXXF8HEAcFwY4dEi2NdKUBayod7YDRU8txHDjmLVE6rNlQFzoP+PnQ+pisoQCQ2kt10XgJju+xaqflJ+SNli9J4rWpCBhowBVcNsZjBokjESCftjRQvYt2trnkW+MIwj0DXg64317gKR2NsK75j41K66BSa7YLfhJSsM0E5f0n3MWU0TRJAiC8+4JvqERifeM8WvnfFb2Jldpzno/1JrBv5Nru06qsQt9zxWW3bKgC2nQ1npJzeR1s5uTGZsFnVVULJicStQXdQo32RSTMTgGsqwH0Q==
        dell-home-server ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIHIF7BIoIAPX1gBffDvuKoHHXrHRbgJdDze6DhH97N5B
        macbook ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILZEeBucsKC/jKItBn8MQxSBSUiO2oMCHn5u5iSxI7Ac
        macbook ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDBkyRxLLxPV4SY0uvlfyLBHNm9z6yorToW4B6LnqmpVd9xHlRxxq/qT99LozZohvSbKIK7JDEaho/hA4ClecyYCsDjOJumxLHXk3+14bYFPpO1BqE0DNdiFza6zlyaCQsuxwWGoWMNDHwgBgpS7XupSYHT4hGocY3lDL0GXKb372CIsgw+qExSAzZ7agQ4mxSHT/54qnO3BEiIEHAvCHmZh18ozVMNu921FHa6IFBp7H68vzk/hsU7vgx41r/5p9IVHdT6oII7gX0pCYTygllxf3jqkXnUoQW4H3C4WtapXbmdfZNXBnVj1iBTNzg2fMKuqeTYPtrieC4x5fD7BUijUnMTJZ6LiyGN85VDw6ELn6uvwhxy2T6COXp+WlFF45BkHUfikx4AcMz3ah+aZ1B/wkAoWorgTeuWnJjIqHR6z4cYeW88j4cZ9ZMnFRvg4XV638Qr5QaNhejAUr2Xcf0ohmtjNt6ZkZXhncX3JVWepp5RloZ+340kKfRt8GJ8mb8=
        macbook ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBCsy6/DavcIK0qddj+r5Hp4RA6LAYl8V9MvGLYxhHe63bT4keA+2kK15EQSuM6TxCy+yaHnh424yMdJC6FAiAYg=
        monstrum ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAII+f73fJcgbVVbfMI+lAzOk582TGp6t7O3jFxk60B8
      '';
    };
  };
}
