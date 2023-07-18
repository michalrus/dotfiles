{ inputs }:

let nixpkgs = inputs.nixpkgs-lenovo-x1; in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit inputs; }; }
    nixpkgs.nixosModules.notDetected
    ./hardware.nix
  ] ++ (with inputs.self.nixosModules; [

    #cups-reenable  # disabled for battery life
    dotfiles-old
    dynamic-profiles
    guest-account
    hibernate-on-low-battery
    lock-vts
    lock-x11-displays
    malicious-hosts
    no-display-manager
    #sane-extra-config
    somagic-easycap

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
    ./features/openvpn-michalrus_com.nix
    ./features/openvpn-nordvpn.nix
    ./features/podman.nix
    ./features/proaudio.nix
    ./features/transmission.nix
    ./features/udev-remap-keyboard.nix
    ./features/window-managers.nix

  ]);
}
