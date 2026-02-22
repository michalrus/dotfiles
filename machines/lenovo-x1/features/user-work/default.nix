{
  pkgs,
  flake,
  ...
}: let
  unfree = import pkgs.path {
    inherit (pkgs.stdenv.hostPlatform) system;
    config.allowUnfree = true;
  };
in {
  users = {
    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      uid = 1337;

      description = "Michal Rus (w)";
      extraGroups = ["audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video"];
      dotfiles-old.profiles = [
        "base"
        "michalrus/base"
        /*
        "git-annex"
        */
        "michalrus/work/iohk"
      ];
      packages = with pkgs; [
        # (hardened-firefox.makeWrapped {
        #   localAutocompletePort = 9999; #config.services.firefox-autocomplete.userPorts.mw;
        #   extraPrefs = hardened-firefox.unwrapped.cfgEnableDRM;
        # })
        unfree.jetbrains.webstorm
        yarn
        nodejs
      ];
    };
  };

  home-manager.users.mw.imports = [
    {
      services.gnome-keyring = {
        enable = true;
        components = ["pkcs11" "secrets"]; # provides org.freedesktop.secrets for `jump-cloud-password-manager`
      };
      home.file.".config/hypr/hyprland.conf".text = ''
        exec-once = systemctl --user start gnome-keyring.service
      '';
      home.packages = [
        flake.packages.${pkgs.stdenv.hostPlatform.system}.jumpcloud-password-manager
        pkgs.seahorse # to manage `gnome-keyring` – you have to create a "login" password keyring in it
        pkgs.gcr # provides `org.gnome.keyring.SystemPrompter` + `gcr-prompter`
      ];
    }
  ];

  #services.firefox-autocomplete.userPorts.mw = 9115;
}
