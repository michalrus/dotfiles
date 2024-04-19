{ flake, config, lib, pkgs, ... }:

{
  users.users.root.hashedPassword = "$6$urdrKHI/CxKbydF5$H6vObqN/e6qhexvgI981Kq.MPcIx8PY0R.UwPveb0M.BATrF1FPFsV/n.ejgjGdmHp1K2h4LP3unbK2IWLGSP0";

  users.users.m = {
    isNormalUser = true;
    description = "Michal Rus";
    hashedPassword = "$6$5KiojYxHlGIjx/0N$u3qd01KYLOFZ0.mSfyxOoeK693TXE/UaKPiAcX/.X75A9KJnI4W/V7u2AIZYi.PW0eMDgNIJiiXGum0aaEgg//";
    extraGroups = [ "wheel" ];
    openssh.authorizedKeys.keyFiles = [ ../../secrets/ssh-key-personal.pub ];
    dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/personal" ];
  };

  users.users.mw = {
    isNormalUser = true;
    description = "Michal Rus (work)";
    openssh.authorizedKeys.keyFiles = [ ../../secrets/ssh-key-work.pub  ];
    dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/work/iohk" ];
  };
}
