{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2505;
in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    nixpkgs.nixosModules.notDetected

    # Idle temperatures go up 60–70 ℃ with this. Without it, around 35 ℃:
    #flake.inputs.nixos-hardware.nixosModules.lenovo-thinkpad-x1-extreme-gen2

    flake.inputs.nixos-hardware.nixosModules.common-gpu-nvidia-disable  # battery life
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
    malicious-hosts
    #sane-extra-config
    somagic-easycap

  ]) ++ [

    ../_shared_/features/fav-pkgs-cli-thin
    ../_shared_/features/immutable-users
    ../_shared_/features/kill-user-processes
    ../_shared_/features/locale-en-iso
    ../_shared_/features/more-entropy
    ../_shared_/features/mtr-traceroute-fping
    ../_shared_/features/nix.conf/work-substituters.nix
    ../_shared_/features/nix.conf
    ../_shared_/features/systemd-accounting
    ../_shared_/features/zsh
    ../_shared_/features/hyprland
    ../_shared_/features/fonts
    ../_shared_/features/desktop-apps
    ../_shared_/features/chromium
    ../_shared_/features/games
    ../_shared_/features/wine

    ./features/android
    ./features/bluetooth
    ./features/fav-pkgs-cli-fat
    ./features/fav-pkgs-desktop
    #./features/firefox-autocomplete
    #./features/hardened-chromium
    #./features/hardened-firefox
    ./features/ledger
    ./features/libvirt
    #./features/mpd
    #./features/musnix
    ./features/openvpn-michalrus_com
    ./features/openvpn-nordvpn
    #./features/podman
    ./features/docker
    ./features/proaudio
    flake.nixosModules.torified-users
    ./features/tor
    ./features/udev-remap-keyboard
    flake.nixosModules.guest-account
    ./features/user-guest
    ./features/user-personal
    ./features/user-root  # TODO: remove
    ./features/user-work
    ./features/yubikey

    flake.inputs.agenix.nixosModules.default
    {
      age.identityPaths = [ "/etc/ssh/ssh_host_ed25519_key" ];
      age.secrets.ssh-key-personal-git-annex = { file = inputs.self + "/secrets/ssh-key-personal-git-annex.age"; owner = "m"; };
      age.secrets.ssh-config-work-devx = { file = inputs.self + "/secrets/ssh-config-work-devx.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-devx = { file = inputs.self + "/secrets/ssh-known_hosts-work-devx.age"; owner = "mw"; };
      age.secrets.ssh-config-work-iog = { file = inputs.self + "/secrets/ssh-config-work-iog.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-iog = { file = inputs.self + "/secrets/ssh-known_hosts-work-iog.age"; owner = "mw"; };
      age.secrets.ssh-config-work-lace = { file = inputs.self + "/secrets/ssh-config-work-lace.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-lace = { file = inputs.self + "/secrets/ssh-known_hosts-work-lace.age"; owner = "mw"; };
      age.secrets.ssh-config-work-blockfrost = { file = inputs.self + "/secrets/ssh-config-work-blockfrost.age"; owner = "mw"; };
      age.secrets.ssh-known_hosts-work-blockfrost = { file = inputs.self + "/secrets/ssh-known_hosts-work-blockfrost.age"; owner = "mw"; };
    }

    flake.inputs.home-manager-2505.nixosModules.home-manager
    ({ config, ... }: {
      home-manager = {
        extraSpecialArgs = { inherit flake; inherit (config.age) secrets; };
        useGlobalPkgs = true;
        useUserPackages = true;
        sharedModules = [
          { home.stateVersion = "23.05"; }

          ../_shared_/home/shells
          ../_shared_/home/gnupg
          ../_shared_/home/git
          ../_shared_/home/password-store
          ../_shared_/home/firefox
          ../_shared_/home/haskell
          ../_shared_/home/gnu-screen
          ../_shared_/home/mpv
          ../_shared_/home/alacritty
        ];
        users.m.imports = [
          ../_shared_/home/identity-personal
          ../_shared_/home/doom-emacs
          #../_shared_/home/zed-editor
        ];
        users.mw.imports = [
          ../_shared_/home/identity-work
          ../_shared_/home/doom-emacs
          #../_shared_/home/zed-editor
        ];
        users.md.imports = [ ];
        users.guest.imports = [
          { home.file.".wallpaper.png".source = ../_shared_/assets/wallpapers/rainbow.png; }
        ];
        users.root.imports = [ ];
      };
    })

    { security.pam.services.su.requireWheel = true; }

    # FIXME: get rid of ↓
    { environment.variables.PATH = [ "$HOME/.bin" ]; }

    #{ boot.binfmt.emulatedSystems = [ "aarch64-linux" ]; }  # for building Raspberry Pi systems on x86_64

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
        powerKey = "suspend";
      };
    }

    {
      services.journald.extraConfig = ''
        SystemMaxUse=200M
      '';
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
