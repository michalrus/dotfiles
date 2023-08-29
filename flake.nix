{

  inputs = {
    nixpkgs-2003.url = "github:NixOS/nixpkgs/nixpkgs-20.03-darwin";

    nixpkgs-2003-michalrus_com = {
      url = "github:NixOS/nixpkgs/cb1996818edf506c0d1665ab147c253c558a7426";
      flake = false; # FIXME: too old
    };

    nixpkgs-2211.url = "github:NixOS/nixpkgs/nixpkgs-22.11-darwin";
    nixpkgs-2305.url = "github:NixOS/nixpkgs/nixpkgs-23.05-darwin";

    nix-darwin-2305.url = "github:lnl7/nix-darwin/master";
    nix-darwin-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    home-manager-2305.url = "github:nix-community/home-manager/release-23.05";
    home-manager-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    doom-emacs = { url = "github:doomemacs/doomemacs"; flake = false; };

    nix-doom-emacs-2305.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

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

      # nixos-rebuild switch -L --keep-going --flake .#michalrus_com --target-host root@michalrus.com
      michalrus_com = import ./machines/michalrus_com { inherit inputs; };
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
      vocproc = callPackage ./packages/vocproc { inherit lv2-cpp-tools; };
      yt-dlp = callPackage ./packages/yt-dlp { flake = inputs.self; };
      x11-rootless = callPackage ./packages/x11-rootless {};
      x11-screenshot = callPackage ./packages/x11-screenshot {};
    });

  };

}
