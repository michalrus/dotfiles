{

  inputs = {
    nixpkgs-2211.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    nixpkgs-2305.url = "github:NixOS/nixpkgs/nixos-23.05";

    nixpkgs-2311.url = "github:NixOS/nixpkgs/nixos-23.11";

    # Chromium, yt-dlp etc.
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # A preview PR, to build a few things that require MacOS SDK ≥13.0:
    nixpkgs-macos-sdk-13.url = "github:NixOS/nixpkgs/pull/229210/head";

    nix-darwin-2311.url = "github:lnl7/nix-darwin/master";
    nix-darwin-2311.inputs.nixpkgs.follows = "nixpkgs-2311";

    home-manager-2305.url = "github:nix-community/home-manager/release-23.05";
    home-manager-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    home-manager-2311.url = "github:nix-community/home-manager/release-23.11";
    home-manager-2311.inputs.nixpkgs.follows = "nixpkgs-2311";

    agenix.url = "github:ryantm/agenix";
    agenix.inputs.nixpkgs.follows = "nixpkgs-2305";
    agenix.inputs.darwin.follows = "nix-darwin-2311";
    agenix.inputs.home-manager.follows = "home-manager-2305";

    doom-emacs = { url = "github:doomemacs/doomemacs"; flake = false; };

    yt-dlp = { url = "github:yt-dlp/yt-dlp"; flake = false; };

    malicious-hosts = {
      # Intentionally referenced as a file under a mutable link for semi-auto updates:
      url = "https://github.com/StevenBlack/hosts/raw/master/alternates/fakenews-gambling/hosts";
      flake = false;
    };

    cloudflare-ips-v4 = { url = "https://www.cloudflare.com/ips-v4"; flake = false; };
    cloudflare-ips-v6 = { url = "https://www.cloudflare.com/ips-v6"; flake = false; };
  };

  outputs = inputs: {

    nixosConfigurations = {
      # nixos-rebuild switch -L --keep-going --flake .#aneta --target-host root@10.77.2.1
      aneta = import ./machines/aneta { inherit inputs; };

      # sudo nixos-rebuild switch -L --keep-going --flake .#lenovo-x1
      lenovo-x1 = import ./machines/lenovo-x1 { inherit inputs; };

      # sudo nixos-rebuild switch -L --keep-going --flake .#macbook-nixos
      macbook-nixos = import ./machines/macbook-nixos { inherit inputs; };

      # nixos-rebuild switch -L --keep-going --flake .#michalrus_com --target-host root@michalrus.com
      michalrus_com = import ./machines/michalrus_com { inherit inputs; };

      # nixos-rebuild switch -L --keep-going --flake .#dell-home-server --target-host root@10.77.2.11
      dell-home-server = import ./machines/dell-home-server { inherit inputs; };
    };

    darwinConfigurations = rec {
      # darwin-rebuild switch -L --keep-going --flake .#macbook
      macbook = import ./machines/macbook { inherit inputs; };
    };

    nixosModules = {
      cups-reenable = import ./modules/cups-reenable;
      dotfiles-old = import ./modules/dotfiles-old;
      firewall-comments = import ./modules/firewall-comments;
      gnu-screen = import ./modules/gnu-screen;
      guest-account = import ./modules/guest-account;
      hibernate-on-low-battery = import ./modules/hibernate-on-low-battery;
      lock-vts = import ./modules/lock-vts { on-vt-switch-src = ./packages/on-vt-switch; };
      lock-x11-displays = import ./modules/lock-x11-displays;
      malicious-hosts = import ./modules/malicious-hosts { inherit (inputs) malicious-hosts; };
      nonet-group = import ./modules/nonet-group;
      sane-extra-config = import ./modules/sane-extra-config;
      somagic-easycap = import ./modules/somagic-easycap;
      sqlite-dump = import ./modules/sqlite-dump;
      torified-users = import ./modules/torified-users;
      update-raspberry-pi-firmware = import ./modules/update-raspberry-pi-firmware;
    };

    lib = import ./lib { inherit inputs; };

    packages = inputs.nixpkgs-2305.lib.genAttrs [
      "x86_64-linux"
      "aarch64-linux"
      "x86_64-darwin"
      "aarch64-darwin"
    ] (system: let
      inherit (inputs.nixpkgs-2305.legacyPackages.${system}) callPackage;
    in inputs.self.lib.filterSystem system rec {
      autotalent = callPackage ./packages/autotalent {};
      cp2104-gpio = callPackage ./packages/cp2104-gpio {};
      dmenu-is-rofi = callPackage ./packages/dmenu-is-rofi {};
      doom-emacs = callPackage ./packages/doom-emacs { inherit (inputs) doom-emacs; };
      doom-emacs-mine = callPackage ./packages/doom-emacs-mine { inherit doom-emacs; };
      git-annex-hacks = callPackage ./packages/git-annex-hacks {};
      gettext-emacs = callPackage ./packages/gettext-emacs {};
      gregorio = callPackage ./packages/gregorio {};
      lv2-cpp-tools = callPackage ./packages/lv2-cpp-tools {};
      on-vt-switch = callPackage ./packages/on-vt-switch {};
      noise = callPackage ./packages/noise {};
      pms5003 = callPackage ./packages/pms5003 {};
      rofi-unicode-input = callPackage ./packages/rofi-unicode-input {};
      talentedhack = callPackage ./packages/talentedhack {};
      tap-plugins = callPackage ./packages/tap-plugins {};
      transcribe = callPackage ./packages/transcribe {};
      vftool = inputs.nixpkgs-2311.legacyPackages.${system}.callPackage ./packages/vftool { inherit (inputs) nixpkgs-macos-sdk-13; };
      vocproc = callPackage ./packages/vocproc { inherit lv2-cpp-tools; };
      yt-dlp = inputs.nixpkgs-unstable.legacyPackages.${system}.callPackage ./packages/yt-dlp { flake = inputs.self; };
      x11-rootless = callPackage ./packages/x11-rootless {};
      x11-screenshot = callPackage ./packages/x11-screenshot {};
    });

  };

}
