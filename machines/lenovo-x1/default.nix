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
    guest-account
    hibernate-on-low-battery
    lock-vts
    lock-x11-displays
    malicious-hosts
    no-display-manager
    nonet-group
    #sane-extra-config
    somagic-easycap

    sqlite-dump
    {
      services.sqlite-dump = [{
        source = "/home/m/.shared/nofatty/data.db";
        destination = "/home/m/Archive/Personal/Backup/nofatty.sql";
        runAt = "*:0/15"; # every 15 mins
        user = "m";
        group = "users";
      }];
    }

    torified-users
    ./features/tor.nix

    ./features/firefox-autocomplete.nix
    { services.firefox-autocomplete.userPorts.m = 9114; }
    { services.firefox-autocomplete.userPorts.mw = 9115; }

    ./features/android.nix
    ./features/emacs.nix
    ./features/hardened-chromium.nix
    ./features/hardened-firefox.nix
    ./features/libvirt.nix
    #./features/mpd.nix
    #./features/musnix.nix
    ./features/old-base.nix  # TODO
    ./features/old-base-mine.nix  # TODO
    ./features/openvpn-michalrus_com.nix
    ./features/openvpn-nordvpn.nix
    ./features/podman.nix
    ./features/proaudio.nix
    ./features/transmission.nix
    ./features/udev-remap-keyboard.nix
    ./features/window-managers.nix

  ]);
}
