{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-lenovo-x1;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    nixpkgs.nixosModules.notDetected
    ./hardware.nix

    { networking.hostName = "lenovo-x1"; }
    { time.timeZone = "Europe/Warsaw"; }

  ] ++ (with flake.nixosModules; [

    #cups-reenable  # disabled for battery life
    dotfiles-old
    dynamic-profiles
    #gnu-screen  # unused
    hibernate-on-low-battery
    lock-vts
    lock-x11-displays
    malicious-hosts
    no-display-manager
    nonet-group
    #sane-extra-config
    somagic-easycap

  ]) ++ [

    ../common-features/nix.conf.nix
    ../common-features/nix.conf-work-substituters.nix
    ../common-features/locale-en-iso.nix
    ../common-features/systemd-accounting.nix

    ./features/android.nix
    ./features/bluetooth.nix
    ./features/emacs.nix
    ./features/fav-pkgs-cli.nix
    ./features/fav-pkgs-desktop.nix
    ./features/firefox-autocomplete.nix
    ./features/hardened-chromium.nix
    ./features/hardened-firefox.nix
    ./features/ledger.nix
    ./features/libvirt.nix
    #./features/mpd.nix
    #./features/musnix.nix
    ./features/old-base.nix  # TODO
    ./features/openvpn-michalrus_com.nix
    ./features/openvpn-nordvpn.nix
    ./features/podman.nix
    ./features/proaudio.nix
    flake.nixosModules.torified-users
    ./features/tor.nix
    ./features/transmission.nix
    ./features/udev-remap-keyboard.nix
    flake.nixosModules.guest-account
    ./features/user-guest.nix
    ./features/user-personal.nix
    ./features/user-work.nix
    ./features/window-managers.nix
    ./features/yubikey.nix

    { boot.binfmt.emulatedSystems = [ "aarch64-linux" ]; }  # for building Raspberry Pi systems on x86_64

    { networking.firewall.allowedTCPPorts = [ 12345 ]; }  # python -m http.server 12345

    { programs.ssh.startAgent = false; } # using gpg-agent as ssh-agent

    {
      networking.networkmanager = {
        enable = true;
        dhcp = "dhclient"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
      };
    }

    {
      services.logind = {
        lidSwitch = "suspend";
        extraConfig = ''
          HandlePowerKey=suspend
        '';
      };
    }

    flake.nixosModules.sqlite-dump
    {
      services.sqlite-dump = [{
        source = "/home/m/.shared/nofatty/data.db";
        destination = "/home/m/Archive/Personal/Backup/nofatty.sql";
        runAt = "*:0/15"; # every 15 mins
        user = "m";
        group = "users";
      }];
    }

  ];
}
