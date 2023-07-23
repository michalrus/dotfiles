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
    #sane-extra-config
    somagic-easycap

  ]) ++ [

    ../common-features/fav-pkgs-cli-thin.nix
    ../common-features/immutable-users.nix
    ../common-features/kill-user-processes.nix
    ../common-features/locale-en-iso.nix
    ../common-features/more-entropy.nix
    ../common-features/mtr-traceroute-fping.nix
    ../common-features/nix.conf-work-substituters.nix
    ../common-features/nix.conf.nix
    ../common-features/systemd-accounting.nix
    ../common-features/zsh.nix

    ./features/android.nix
    ./features/bluetooth.nix
    ./features/emacs.nix
    ./features/fav-pkgs-cli-fat.nix
    ./features/fav-pkgs-desktop.nix
    ./features/firefox-autocomplete.nix
    ./features/hardened-chromium.nix
    ./features/hardened-firefox.nix
    ./features/ledger.nix
    ./features/libvirt.nix
    #./features/mpd.nix
    #./features/musnix.nix
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
    ./features/user-root.nix  # TODO: remove
    ./features/user-work.nix
    ./features/window-managers.nix
    ./features/wine.nix
    ./features/yubikey.nix

    { security.pam.services.su.requireWheel = true; }

    # FIXME: get rid of ↓
    { environment.variables.PATH = [ "$HOME/.bin" ]; }

    { boot.binfmt.emulatedSystems = [ "aarch64-linux" ]; }  # for building Raspberry Pi systems on x86_64

    { networking.firewall.allowedTCPPorts = [ 12345 ]; }  # python -m http.server 12345

    { programs.ssh.startAgent = false; } # using gpg-agent as ssh-agent

    flake.nixosModules.nonet-group
    { networking.firewall.nonetGroup.enable = true; }

    {
      networking.networkmanager = {
        enable = true;
        dhcp = "dhclient"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
        dns = "none";
      };
      networking.nameservers = [
        "1.1.1.1"
        "1.0.0.1"
      ];
    }

    { hardware.sane.enable = true; }

    {
      services.logind = {
        lidSwitch = "suspend";
        extraConfig = ''
          HandlePowerKey=suspend
        '';
      };
    }

    {
      services.journald.extraConfig = ''
        SystemMaxUse=200M
      '';
    }

    ({ pkgs, ...}: {
      services.printing = {
        enable = true;
        drivers = with pkgs; [ gutenprint hplip epson-escpr ];
      };
    })

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
