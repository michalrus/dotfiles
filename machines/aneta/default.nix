{ inputs }:

let
  flake = inputs.self;
  nixpkgs = inputs.nixpkgs-2003;
in

nixpkgs.lib.nixosSystem {
  system = "aarch64-linux";
  modules = [
    { _module.args = { inherit flake; }; }
    #nixpkgs.nixosModules.notDetected
    ./hardware.nix

    { networking.hostName = "aneta"; }
    { time.timeZone = "UTC"; }

    flake.nixosModules.dotfiles-old
    flake.nixosModules.gnu-screen

    ../common-features/fav-pkgs-cli-thin.nix
    ../common-features/immutable-users.nix
    ../common-features/ip-reject-not-drop.nix
    ../common-features/kill-user-processes.nix
    ../common-features/locale-en-iso.nix
    ../common-features/more-entropy.nix
    ../common-features/mtr-traceroute-fping.nix
    ../common-features/nix.conf.nix
    ../common-features/systemd-accounting.nix
    ../common-features/zsh.nix

    ./features/dns.nix
    ./features/nat.nix
    ./features/openvpn.nix
    #./features/proxy.nix  # TODO: needs more work
    ./features/users.nix

    {
      services.journald.extraConfig = ''
        SystemMaxUse=200M
      '';
    }
  ];
}
