{
  inputs = {
    # nixpkgs-2305.url = "github:NixOS/nixpkgs/nixos-23.05";

    nixpkgs-2311.url = "github:NixOS/nixpkgs/nixos-23.11";

    # nixpkgs-2405.url = "github:NixOS/nixpkgs/nixos-24.05";

    nixpkgs-2411.url = "github:NixOS/nixpkgs/nixos-24.11";

    # nixpkgs-2505.url = "github:NixOS/nixpkgs/nixos-25.05";

    nixpkgs-2511.url = "github:NixOS/nixpkgs/nixos-25.11";

    # Chromium, yt-dlp etc.
    nixpkgs-unstable.url = "github:NixOS/nixpkgs/nixos-unstable";

    # A preview PR, to build a few things that require MacOS SDK ≥13.0:
    nixpkgs-macos-sdk-13.url = "github:NixOS/nixpkgs/pull/229210/head";

    # nix-darwin-2311.url = "github:lnl7/nix-darwin/master";
    # nix-darwin-2311.inputs.nixpkgs.follows = "nixpkgs-2311";

    nix-darwin-2511.url = "github:nix-darwin/nix-darwin/nix-darwin-25.11";
    nix-darwin-2511.inputs.nixpkgs.follows = "nixpkgs-2511";

    # home-manager-2305.url = "github:nix-community/home-manager/release-23.05";
    # home-manager-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    # home-manager-2311.url = "github:nix-community/home-manager/release-23.11";
    # home-manager-2311.inputs.nixpkgs.follows = "nixpkgs-2311";

    # home-manager-2405.url = "github:nix-community/home-manager/release-24.05";
    # home-manager-2405.inputs.nixpkgs.follows = "nixpkgs-2405";

    # home-manager-2411.url = "github:nix-community/home-manager/release-24.11";
    # home-manager-2411.inputs.nixpkgs.follows = "nixpkgs-2411";

    # home-manager-2505.url = "github:nix-community/home-manager/release-25.05";
    # home-manager-2505.inputs.nixpkgs.follows = "nixpkgs-2505";

    home-manager-2511.url = "github:nix-community/home-manager/release-25.11";
    home-manager-2511.inputs.nixpkgs.follows = "nixpkgs-2511";

    nixos-hardware.url = "github:NixOS/nixos-hardware";

    agenix = {
      url = "github:ryantm/agenix";
      inputs = {
        nixpkgs.follows = "nixpkgs-2511";
        darwin.follows = "nix-darwin-2511";
        home-manager.follows = "home-manager-2511";
      };
    };

    treefmt-nix = {
      url = "github:numtide/treefmt-nix";
      inputs.nixpkgs.follows = "nixpkgs-2511";
    };

    doom-emacs = {
      url = "github:doomemacs/doomemacs";
      flake = false;
    };

    yt-dlp = {
      url = "github:yt-dlp/yt-dlp";
      flake = false;
    };

    malicious-hosts = {
      # Intentionally referenced as a file under a mutable link for semi-auto updates:
      url = "https://github.com/StevenBlack/hosts/raw/master/alternates/fakenews-gambling/hosts";
      flake = false;
    };

    cloudflare-ips-v4 = {
      url = "https://www.cloudflare.com/ips-v4";
      flake = false;
    };
    cloudflare-ips-v6 = {
      url = "https://www.cloudflare.com/ips-v6";
      flake = false;
    };

    bun2nix = {
      url = "github:nix-community/bun2nix";
      inputs.nixpkgs.follows = "nixpkgs-2511";
      inputs.treefmt-nix.follows = "treefmt-nix";
    };

    serena = {
      url = "github:oraios/serena/main";
      inputs.nixpkgs.follows = "nixpkgs-unstable";
    };
  };

  outputs = inputs: {
    nixosConfigurations = {
      # nixos-rebuild switch -L --keep-going --flake .#aneta --target-host root@10.77.2.1
      aneta = import ./machines/aneta {inherit inputs;};

      # sudo nixos-rebuild switch -L --keep-going --flake .#lenovo-x1
      lenovo-x1 = import ./machines/lenovo-x1 {inherit inputs;};

      # sudo nixos-rebuild switch -L --keep-going --flake .#macbook-nixos
      macbook-nixos = import ./machines/macbook-nixos {inherit inputs;};

      # nixos-rebuild switch -L --keep-going --flake .#michalrus_com --target-host root@michalrus.com
      michalrus_com = import ./machines/michalrus_com {inherit inputs;};

      # nixos-rebuild switch -L --keep-going --flake .#dell-home-server --target-host root@10.77.2.11
      dell-home-server = import ./machines/dell-home-server {inherit inputs;};

      # nixos-rebuild switch -L --keep-going --flake .#dell-home-server --target-host root@10.77.2.12
      monstrum = import ./machines/monstrum {inherit inputs;};
    };

    darwinConfigurations = {
      # darwin-rebuild switch -L --keep-going --flake .#macbook
      macbook = import ./machines/macbook {inherit inputs;};
    };

    nixosModules = {
      cups-reenable = import ./modules/cups-reenable;
      dotfiles-old = import ./modules/dotfiles-old;
      firewall-comments = import ./modules/firewall-comments;
      gnu-screen = import ./modules/gnu-screen;
      guest-account = import ./modules/guest-account;
      hibernate-on-low-battery = import ./modules/hibernate-on-low-battery;
      lock-vts = import ./modules/lock-vts {on-vt-switch-src = ./packages/on-vt-switch;};
      malicious-hosts = import ./modules/malicious-hosts {inherit (inputs) malicious-hosts;};
      nonet-group = import ./modules/nonet-group;
      sane-extra-config = import ./modules/sane-extra-config;
      somagic-easycap = import ./modules/somagic-easycap;
      sqlite-dump = import ./modules/sqlite-dump;
      torified-users = import ./modules/torified-users;
      update-raspberry-pi-firmware = import ./modules/update-raspberry-pi-firmware;
    };

    lib = import ./lib {inherit inputs;};

    packages =
      inputs.nixpkgs-2511.lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
        "x86_64-darwin"
        "aarch64-darwin"
      ] (system: let
        inherit (inputs.nixpkgs-2511.legacyPackages.${system}) callPackage;
      in
        inputs.self.lib.filterSystem system rec {
          accuradio = callPackage ./packages/accuradio {};
          amscope-amlite = callPackage ./packages/amscope-amlite {};
          autotalent = callPackage ./packages/autotalent {};
          cp2104-gpio = callPackage ./packages/cp2104-gpio {};
          dmenu-is-rofi = callPackage ./packages/dmenu-is-rofi {};
          doom-emacs = callPackage ./packages/doom-emacs {inherit (inputs) doom-emacs;};
          doom-emacs-mine = callPackage ./packages/doom-emacs-mine {inherit doom-emacs;};
          git-annex-hacks = callPackage ./packages/git-annex-hacks {};
          gettext-emacs = callPackage ./packages/gettext-emacs {};
          gregorio = callPackage ./packages/gregorio {};
          hunspell-dictionaries-chromium-pl = callPackage ./packages/hunspell-dictionaries-chromium-pl {};
          hyprland-screenshot = callPackage ./packages/hyprland-screenshot {};
          jumpcloud-password-manager = callPackage ./packages/jumpcloud-password-manager {};
          lv2-cpp-tools = callPackage ./packages/lv2-cpp-tools {};
          nixlint = callPackage ./packages/nixlint {};
          opencode-bwrap = callPackage ./packages/opencode-bwrap {
            inherit (inputs) nixpkgs-unstable;
            bun2nix = inputs.bun2nix.packages.${system}.default;
            serena = inputs.serena.packages.${system}.default;
          };
          on-vt-switch = callPackage ./packages/on-vt-switch {};
          naps2 = callPackage ./packages/naps2 {};
          noise = callPackage ./packages/noise {};
          pms5003 = callPackage ./packages/pms5003 {};
          qemu-win10 = callPackage ./packages/qemu-win10 {};
          radio = callPackage ./packages/radio {
            inherit accuradio radio-chillhop;
          };
          radio-chillhop = callPackage ./packages/radio-chillhop {};
          rofi-unicode-input = callPackage ./packages/rofi-unicode-input {};
          talentedhack = callPackage ./packages/talentedhack {};
          tap-plugins = callPackage ./packages/tap-plugins {};
          transcribe = callPackage ./packages/transcribe {};
          wayland-logout = callPackage ./packages/wayland-logout {};
          wayland-unicode-input = callPackage ./packages/wayland-unicode-input {};
          wayland-emoji-input = callPackage ./packages/wayland-unicode-input {onlyEmoji = true;};
          wayland-random-input = callPackage ./packages/wayland-random-input {};
          wine-bwrap = callPackage ./packages/wine-bwrap {};
          # FIXME:
          vftool = inputs.nixpkgs-2311.legacyPackages.${system}.callPackage ./packages/vftool {inherit (inputs) nixpkgs-macos-sdk-13;};
          vocproc = callPackage ./packages/vocproc {inherit lv2-cpp-tools;};
          yt-dlp = inputs.nixpkgs-unstable.legacyPackages.${system}.callPackage ./packages/yt-dlp {flake = inputs.self;};
          #zed-editor = inputs.nixpkgs-unstable.legacyPackages.${system}.callPackage ./packages/zed-editor { flake = inputs.self; };
        });
  };
}
