{
  config,
  pkgs,
  lib,
  secrets,
  ...
}: {
  programs.git = {
    settings.user.email = "michal.rus@iohk.io";
    settings.user.name = "Michal Rus";
    signing.key = "michal.rus@iohk.io";
    signing.signByDefault = true;
  };

  home.file.".gnupg/sshcontrol".text = ''
    B05520F6A1ECEE40238CEC10DE835DEA6D5089A1
  '';

  programs.gpg.settings.default-key = "michal.rus@iohk.io";

  home.file.".avatar.jpg".source = ../../assets/avatar.jpg;
  home.file.".wallpaper.png".source = ../../assets/wallpapers/yosemite.png;

  home.file.".ssh/config.d/dev-machines".text = ''
    Host macbook 10.77.2.99
      Hostname 10.77.2.99
      HostKeyAlias macbook
      UserKnownHostsFile ~/.ssh/known_hosts.d/dev-machines
      User mw

    Host macbook-nixos
      ${if pkgs.stdenv.hostPlatform.system == "aarch64-linux" then "#" else ""}ProxyJump macbook
      Hostname 192.168.65.2
      HostKeyAlias macbook-nixos
      UserKnownHostsFile ~/.ssh/known_hosts.d/dev-machines
      User mw

    Host win10
      Hostname 192.168.122.241
      HostKeyAlias win10
      UserKnownHostsFile ~/.ssh/known_hosts.d/dev-machines
      User mw
  '';

  home.file.".ssh/known_hosts.d/dev-machines".text = ''
    macbook ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAILZEeBucsKC/jKItBn8MQxSBSUiO2oMCHn5u5iSxI7Ac
    macbook ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQDBkyRxLLxPV4SY0uvlfyLBHNm9z6yorToW4B6LnqmpVd9xHlRxxq/qT99LozZohvSbKIK7JDEaho/hA4ClecyYCsDjOJumxLHXk3+14bYFPpO1BqE0DNdiFza6zlyaCQsuxwWGoWMNDHwgBgpS7XupSYHT4hGocY3lDL0GXKb372CIsgw+qExSAzZ7agQ4mxSHT/54qnO3BEiIEHAvCHmZh18ozVMNu921FHa6IFBp7H68vzk/hsU7vgx41r/5p9IVHdT6oII7gX0pCYTygllxf3jqkXnUoQW4H3C4WtapXbmdfZNXBnVj1iBTNzg2fMKuqeTYPtrieC4x5fD7BUijUnMTJZ6LiyGN85VDw6ELn6uvwhxy2T6COXp+WlFF45BkHUfikx4AcMz3ah+aZ1B/wkAoWorgTeuWnJjIqHR6z4cYeW88j4cZ9ZMnFRvg4XV638Qr5QaNhejAUr2Xcf0ohmtjNt6ZkZXhncX3JVWepp5RloZ+340kKfRt8GJ8mb8=
    macbook ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBCsy6/DavcIK0qddj+r5Hp4RA6LAYl8V9MvGLYxhHe63bT4keA+2kK15EQSuM6TxCy+yaHnh424yMdJC6FAiAYg=

    macbook-nixos ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIFvTbWdAIUqai95gLUWbzdy0Sj3uGv3a8RF/dkzG4bg6
    macbook-nixos ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAACAQCd6JZceqTDw9CcrSYNlL+zMik9Nk498Kej4U+wciH/Z9f9zoJm2t3ys7q9J1wN/ey+0fMCa02O4u618IU0mzpNX5AKybBGhy5pvP9TJ/eq9sqdni7Eo2rs+PvQWZiLamd8+7Dl3R6lDXs9xo1uPeaFLpwiB2ZF3V/ztohWEBatwTV8y9Bfl8ZoBAzw9wcfHqm+jiz/ZPTcYFmF3OdhV6AQGH0O5FcGg8L/l78TrRz3AyDt82Wu7eUYgXib9isPVHaTM6mhFiMUfZAp4rfXe+J0msriWxOtMfR7nt3wndWWRFMCFAZX5lvOxfqclU0ij1Crq/PJd1JP91ME9duaa3emS0MBMwQH1VhcewV5nU7ppRDakYlk0CMGVeQ+tMnoN2P9PU8PH1GVeXfsSmajQ4zFJw/1vmEaF8Is1+i1opuRDxssm00mGecjm/TBanmh1KEkSWzohZOYsI93xa8mTfaB8uB3Yafcg3jynNZMCOedItN7NwLU4Ge4GVMpbzbrnMim2Psv0oDyUYOLFF36YZR2ZerBlbWt/Dmu2qLEpITCaDMGQxP2s3ddlK1AUZDmCJ8li6aSFYRHnPsuw6bYEeg77nH6QU4LqFObySY3v1hPOHDH2RcbGb6jgHO26bQ8jpkXdGJK8+R5oT6UQb7P7FShFX0iSeKMSS6wWqOgICMxGQ==

    win10 ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIAdAxUZFdVSvMcxpTVB3pRp8hfjbBWmrjCBOg86hGlo9
    win10 ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABgQChH4XDqpQjrBkOuTpz4YjUcZsQpvs4juxCzlVsDtFM+8AKyrO1+bymeSWzZwZ5nB4D4SEpJPQIckeRwIGbsnSOEBiLAXXcMGee2UFiG3fY65IaNXymOY3ARspInqGi4gCWMf78ze4uYAwgodfTOmqbJkRja99rwEKn5iuOXBnndvcdc26IY1NDiQBO1AuYaLCmwEpijozZLMp1F/6Q+D+he8sBJj2frVbSFyPW79vOAy/BqC6P9zLC/QQL0cx5YuMurXEwu07tzDkiFAuyUUdUy7tVwB0+TEl065zFseKm2eD9LXBF31OUcC93wbQRZTfVBehugK1hVTcw98Uim7NGOUWo7pB0rtsfG7McHs9JHHGFUoppSwCwf1vVN7F+YOTsuZXk7dMEVwT3fU5ZJugcIRyguunNAezV35wZLsCsSj9tMOkF3iGb6y6fkLQmivWisFKEfbAizIkKFjT4h85sCIvWbSa48XBb1FkuQvY6oxlKWjqSxhQATRkItEhd+Kk=
    win10 ecdsa-sha2-nistp256 AAAAE2VjZHNhLXNoYTItbmlzdHAyNTYAAAAIbmlzdHAyNTYAAABBBLMdzbO5kG5bvN6lUtZ1EV+sEodRCo+8782GohqiquLaJUVJ3AMqtqtotjUIwyWZJcL9Wn6xoHdAnxHcD7tUOvA=
  '';

  home.file.".ssh/config.d/devx".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-config-work-devx.path;
  home.file.".ssh/known_hosts.d/devx".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-known_hosts-work-devx.path;
  home.file.".ssh/config.d/iog".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-config-work-iog.path;
  home.file.".ssh/known_hosts.d/iog".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-known_hosts-work-iog.path;
  home.file.".ssh/config.d/lace".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-config-work-lace.path;
  home.file.".ssh/known_hosts.d/lace".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-known_hosts-work-lace.path;
  home.file.".ssh/config.d/blockfrost".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-config-work-blockfrost.path;
  home.file.".ssh/known_hosts.d/blockfrost".source = config.lib.file.mkOutOfStoreSymlink secrets.ssh-known_hosts-work-blockfrost.path;
}
