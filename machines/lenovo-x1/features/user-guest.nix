{ flake, config, pkgs, ... }:

let
  unfree-2305 = import flake.inputs.nixpkgs-2305 { inherit (pkgs) system; config.allowUnfree = true; };
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
        unfree-2305.google-chrome
        unfree-2305.unrar
      ];
    };
  };

}
