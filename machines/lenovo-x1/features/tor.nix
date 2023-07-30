{ config, pkgs, ... }:

{

  services.tor = {
    enable = true;
    client.enable = true;
    torifiedUsers = [
      { username = "md"; allowedLocalPorts = [ config.services.firefox-autocomplete.userPorts.md ]; }
    ];
  };

  services.firefox-autocomplete.userPorts.md = 9116;

  users.extraUsers.md = {
    hashedPassword = "$6$YYkKeZ8M56$EZh1jGZbAnPaVKwnQebJ40ojqNShuU3Np2rWiPOOrfA1vXlFPCGbmbVZwOfRyCMU5y83ggkA1/jIG7Zzg6iD10";
    isNormalUser = true;
    uid = 1347;
    description = "Michal Rus (d)";
    extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
    dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/tor" "i3" "emacs" ];
    packages = with pkgs; [
      electrum
      (hardened-firefox.makeWrapped {
        localAutocompletePort = config.services.firefox-autocomplete.userPorts.md;
        extraPrefs = ''
          // Override those for more privacy:
          lockPref("privacy.resistFingerprinting", true);
          lockPref("dom.enable_performance", false);
          lockPref("network.cookie.lifetimePolicy", 2); // The cookie expires at the end of the session.
        '';
      })
    ];
  };

}
