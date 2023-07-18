{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-lenovo-x1.url = "github:NixOS/nixpkgs/b3d86c56c786ad9530f1400adbd4dfac3c42877b"; # nixos-21.11, Jan 25, 2022
    nixpkgs-aneta = {
      url = "github:NixOS/nixpkgs/d6c1b566b770cf4cf0c6d4a693da6bdf28c2c3b0"; # nixos-20.03, May 9, 2020
      flake = false; # FIXME: too old
    };
    yt-dlp = { url = "github:yt-dlp/yt-dlp"; flake = false; };
    danPollock = { url = "http://someonewhocares.org/hosts/zero/hosts"; flake = false; };

    nixpkgsDarwin.url = "github:nixos/nixpkgs/nixpkgs-22.05-darwin";
    nixpkgsDarwinUnstable.url = "github:nixos/nixpkgs/nixpkgs-unstable";

    nix-darwin.url = "github:lnl7/nix-darwin/master";
    nix-darwin.inputs.nixpkgs.follows = "nixpkgsDarwin";

    home-manager.url = "github:nix-community/home-manager/release-22.05";
    home-manager.inputs.nixpkgs.follows = "nixpkgsDarwin";

    nix-doom-emacs.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs.inputs.nixpkgs.follows = "nixpkgsDarwin"; # TODO: or should it say "nixpkgsDarwinUnstable"?
  };

  outputs = inputs: {

    nixosConfigurations = {
      # nixos-rebuild switch -L --flake .#aneta --build-host localhost --target-host root@10.77.2.1
      aneta = import ./machines/aneta { inherit inputs; };

      # sudo nixos-rebuild switch -L --flake .#lenovo-x1
      lenovo-x1 = import ./machines/lenovo-x1 { inherit inputs; };
    };

    darwinConfigurations = rec {
      # darwin-rebuild switch -L --flake .#macbook
      macbook = import ./machines/macbook { inherit inputs; };
    };

    nixosModules = {
      cups-reenable = import ./modules/cups-reenable;
      dotfiles-old = import ./modules/dotfiles-old;
      dynamic-profiles = import ./modules/dynamic-profiles;
      firewall-comments = import ./modules/firewall-comments;
      guest-account = import ./modules/guest-account;
      hibernate-on-low-battery = import ./modules/hibernate-on-low-battery;
      lock-vts = import ./modules/lock-vts { on-vt-switch-src = ./packages/on-vt-switch; };
      lock-x11-displays = import ./modules/lock-x11-displays;
      malicious-hosts = import ./modules/malicious-hosts { inherit (inputs) danPollock; };
      no-display-manager = import ./modules/no-display-manager;  # requires ‘dynamic-profiles’
      sane-extra-config = import ./modules/sane-extra-config;
      somagic-easycap = import ./modules/somagic-easycap;
      torified-users = import ./modules/torified-users;
    };

    packages = inputs.nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux"] (system: let
      inherit (inputs.nixpkgs-lenovo-x1.legacyPackages.${system}) callPackage;
    in rec {
      autotalent = callPackage ./packages/autotalent {};
      cp2104-gpio = callPackage ./packages/cp2104-gpio {};
      dmenu-is-rofi = callPackage ./packages/dmenu-is-rofi {};
      git-annex-hacks = callPackage ./packages/git-annex-hacks {};
      gettext-emacs = callPackage ./packages/gettext-emacs {};
      gregorio = callPackage ./packages/gregorio {};
      lv2-cpp-tools = callPackage ./packages/lv2-cpp-tools {};
      on-vt-switch = callPackage ./packages/on-vt-switch {};
      noise = callPackage ./packages/noise {};
      pms5003 = callPackage ./packages/pms5003 {};
      talentedhack = callPackage ./packages/talentedhack {};
      tap-plugins = callPackage ./packages/tap-plugins {};
      transcribe = callPackage ./packages/transcribe {};
      vocproc = callPackage ./packages/vocproc { inherit lv2-cpp-tools; };
      yt-dlp = import ./packages/yt-dlp { inherit inputs system; };
    });

  };

}
