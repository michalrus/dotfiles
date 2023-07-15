{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";

    nixpkgs-aneta = {
      url = "github:NixOS/nixpkgs/d6c1b566b770cf4cf0c6d4a693da6bdf28c2c3b0"; # nixos-20.03, May 9, 2020
      flake = false; # FIXME: too old
    };
  };

  outputs = inputs: {

    nixosConfigurations = {
      # deploy: nixos-rebuild switch --flake .#aneta --build-host localhost --target-host root@10.77.2.1
      aneta = import ./machines/aneta.nix { inherit inputs; };
    };

  };

}
