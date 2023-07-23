{ flake, config, pkgs, ... }:

let
  unfree-23_05 = import flake.inputs.nixpkgs { inherit (pkgs) system; config.allowUnfree = true; };
in

{

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "video" ];
    };

    users.guest = {
      dotfiles-old.profiles = [ "base" "i3" "michalrus/guest" ];
      packages = with pkgs; [
        unfree-23_05.google-chrome
        unfree-23_05.unrar
      ];
    };
  };

}
