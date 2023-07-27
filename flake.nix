{

  inputs = {
    nixpkgs-2003.url = "github:NixOS/nixpkgs/nixpkgs-20.03-darwin";

    nixpkgs-2003-michalrus_com = {
      url = "github:nixos/nixpkgs/cb1996818edf506c0d1665ab147c253c558a7426";
      flake = false; # FIXME: too old
    };

    nixpkgs-2111.url = "github:NixOS/nixpkgs/nixpkgs-21.11-darwin";

    nixpkgs-2305.url = "github:nixos/nixpkgs/nixpkgs-23.05-darwin";

    nix-darwin-2305.url = "github:lnl7/nix-darwin/master";
    nix-darwin-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    home-manager-2305.url = "github:nix-community/home-manager/release-23.05";
    home-manager-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    nix-doom-emacs-2305.url = "github:nix-community/nix-doom-emacs";
    nix-doom-emacs-2305.inputs.nixpkgs.follows = "nixpkgs-2305";

    yt-dlp = { url = "github:yt-dlp/yt-dlp"; flake = false; };

    danPollock = { url = "http://someonewhocares.org/hosts/zero/hosts"; flake = false; };

    cloudflare-ips-v4 = { url = "https://www.cloudflare.com/ips-v4"; flake = false; };
    cloudflare-ips-v6 = { url = "https://www.cloudflare.com/ips-v6"; flake = false; };
  };

  outputs = inputs: {

    nixosConfigurations = {
      # nixos-rebuild switch -L --keep-going --flake .#aneta --build-host localhost --target-host root@10.77.2.1
      aneta = import ./machines/aneta { inherit inputs; };

      # sudo nixos-rebuild switch -L --keep-going --flake .#lenovo-x1
      lenovo-x1 = import ./machines/lenovo-x1 { inherit inputs; };

      # nixos-rebuild switch -L --keep-going --flake .#michalrus_com --build-host localhost --target-host root@michalrus.com
      michalrus_com = import ./machines/michalrus_com { inherit inputs; };
    };

    darwinConfigurations = rec {
      # darwin-rebuild switch -L --keep-going --flake .#macbook
      macbook = import ./machines/macbook { inherit inputs; };
    };

    nixosModules = {
      cups-reenable = import ./modules/cups-reenable;
      dotfiles-old = import ./modules/dotfiles-old;
      dynamic-profiles = import ./modules/dynamic-profiles;
      firewall-comments = import ./modules/firewall-comments;
      gnu-screen = import ./modules/gnu-screen;
      guest-account = import ./modules/guest-account;
      hibernate-on-low-battery = import ./modules/hibernate-on-low-battery;
      lock-vts = import ./modules/lock-vts { on-vt-switch-src = ./packages/on-vt-switch; };
      lock-x11-displays = import ./modules/lock-x11-displays;
      malicious-hosts = import ./modules/malicious-hosts { inherit (inputs) danPollock; };
      no-display-manager = import ./modules/no-display-manager;  # requires ‘dynamic-profiles’
      nonet-group = import ./modules/nonet-group;
      sane-extra-config = import ./modules/sane-extra-config;
      somagic-easycap = import ./modules/somagic-easycap;
      sqlite-dump = import ./modules/sqlite-dump;
      torified-users = import ./modules/torified-users;
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
      yt-dlp = callPackage ./packages/yt-dlp { flake = inputs.self; };
    });

  };

}
