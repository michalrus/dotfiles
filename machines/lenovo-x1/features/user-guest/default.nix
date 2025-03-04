{ config, pkgs, ... }:

let
  unfree = import pkgs.path { inherit (pkgs) system; config.allowUnfree = true; };
in

{

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "video" ];
    };

    users.guest = {
      dotfiles-old.profiles = [ "base" "michalrus/guest" ];
      packages = with pkgs; [
        #unfree.google-chrome  # FIXME: remove, chromium can now do widevine
        unfree.unrar
      ];
    };
  };

}
