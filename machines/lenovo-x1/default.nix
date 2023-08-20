{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2305;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    nixpkgs.nixosModules.notDetected
    ./hardware.nix

    { networking.hostName = "lenovo-x1"; }
    { time.timeZone = "Europe/Warsaw"; }
    { system.stateVersion = "23.05"; }

  ] ++ (with flake.nixosModules; [

    #cups-reenable  # disabled for battery life
    dotfiles-old
    #gnu-screen  # unused
    hibernate-on-low-battery
    lock-vts
    lock-x11-displays
    malicious-hosts
    #sane-extra-config
    somagic-easycap

  ]) ++ [

    (import ../_shared_/features/canoscan-lide-20.nix { inherit flake; })

    ../_shared_/features/fav-pkgs-cli-thin.nix
    ../_shared_/features/immutable-users.nix
    ../_shared_/features/kill-user-processes.nix
    ../_shared_/features/locale-en-iso.nix
    ../_shared_/features/more-entropy.nix
    ../_shared_/features/mtr-traceroute-fping.nix
    ../_shared_/features/nix.conf-work-substituters.nix
    ../_shared_/features/nix.conf.nix
    ../_shared_/features/systemd-accounting.nix
    ../_shared_/features/zsh.nix

    ./features/android.nix
    ./features/bluetooth.nix
    ./features/emacs.nix
    ./features/fav-pkgs-cli-fat.nix
    ./features/fav-pkgs-desktop.nix
    ./features/firefox-autocomplete.nix
    ./features/fonts.nix
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

    flake.inputs.home-manager-2305.nixosModules.home-manager
    {
      home-manager = {
        extraSpecialArgs = { inherit flake; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          ../_shared_/home/shells.nix
          ../_shared_/home/gnupg.nix
          ../_shared_/home/git.nix
          ../_shared_/home/password-store.nix
          ../_shared_/home/emacs.nix
          ../_shared_/home/haskell.nix
          ../_shared_/home/gnu-screen.nix
          ../_shared_/home/mpv.nix
          ./home/shared.nix
        ];
        users.m.imports = [
          ../_shared_/home/identity-personal.nix
          ../_shared_/home/doom-emacs
        ];
        users.mw.imports = [
          ../_shared_/home/identity-work.nix
          ../_shared_/home/doom-emacs
        ];
        users.md.imports = [ ];
        users.guest.imports = [ ./home/guest.nix ];
        users.root.imports = [ ];
      };
    }

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
        dhcp = "dhcpcd"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
        dns = "none";
      };
      networking.nameservers = [
        "1.1.1.1"
        "1.0.0.1"
      ];
    }

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
