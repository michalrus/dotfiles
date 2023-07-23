{ flake, config, lib, pkgs, ... }:

let
  unfree-23_05 = import flake.inputs.nixpkgs { inherit (pkgs) system; config.allowUnfree = true; };
in

{
  networking.networkmanager = {
    enable = true;
    dhcp = "dhclient"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
  };

  programs.ssh.startAgent = false;  # using gpg-agent as ssh-agent

  hardware.pulseaudio = {
    enable = true;
    systemWide = true; # Running multiple concurrent user sessions on different TTYs, I want their audio mixed.
    support32Bit = true;
  };

  # For system-wide PulseAudio: <https://github.com/NixOS/nixpkgs/issues/114399>
  system.activationScripts.fix-pulse-permissions = ''
    chmod 755 /run/pulse
  '';

  systemd.extraConfig = ''
    DefaultCPUAccounting=yes
    DefaultBlockIOAccounting=yes
    DefaultMemoryAccounting=yes
    DefaultTasksAccounting=yes
    DefaultIPAccounting=yes
  '';

  services = {
    logind.lidSwitch = "suspend";
    logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    # No global X11! See <./modules/no-display-manager/i3.nix>
    xserver.enable = lib.mkForce false;
  };

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

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;

      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
      dotfiles-old.base = "${config.users.users.m.home}/.dotfiles/dotfiles";
      dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages = with pkgs; [
        (hardened-firefox.makeWrapped {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.m;
          extraPrefs = hardened-firefox.unwrapped.cfgEnableDRM;
        })
      ];
    };

    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      uid = 1337;

      description = "Michal Rus (w)";
      extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
      dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/work/iohk" "i3" "emacs" ];
      packages = with pkgs; [
        (hardened-firefox.makeWrapped {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.mw;
          extraPrefs = hardened-firefox.unwrapped.cfgEnableDRM;
        })
        unfree-23_05.jetbrains.webstorm
        yarn
        nodejs
      ];
    };
  };

  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };
}
