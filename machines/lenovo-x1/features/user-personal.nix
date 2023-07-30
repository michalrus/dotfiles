{ config, pkgs, ... }:

{

  users = {
    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;

      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
      dotfiles-old.base = "${config.users.users.m.home}/.dotfiles/dotfiles";
      dotfiles-old.profiles = [ "base" "michalrus/base" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages = with pkgs; [
        (hardened-firefox.makeWrapped {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.m;
          extraPrefs = hardened-firefox.unwrapped.cfgEnableDRM;
        })
      ];
    };
  };

  services.firefox-autocomplete.userPorts.m = 9114;

  # FIXME: get rid of this
  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };

}
