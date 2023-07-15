{

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.05";
    nixpkgs-lenovo-x1.url = "github:NixOS/nixpkgs/b3d86c56c786ad9530f1400adbd4dfac3c42877b"; # nixos-21.11, Jan 25, 2022

    nixpkgs-aneta = {
      url = "github:NixOS/nixpkgs/d6c1b566b770cf4cf0c6d4a693da6bdf28c2c3b0"; # nixos-20.03, May 9, 2020
      flake = false; # FIXME: too old
    };
  };

  outputs = inputs: {

    nixosConfigurations = {
      # deploy: nixos-rebuild switch --flake .#aneta --build-host localhost --target-host root@10.77.2.1
      aneta = import ./machines/aneta { inherit inputs; };

      # deploy: sudo nixos-rebuild switch --flake .#lenovo-x1
      lenovo-x1 = import ./machines/lenovo-x1 { inherit inputs; };
    };

    packages = inputs.nixpkgs.lib.genAttrs ["x86_64-linux" "aarch64-linux"] (system: let
      pkgs = inputs.nixpkgs-lenovo-x1.legacyPackages.${system};
    in {
      autotalent = pkgs.callPackage ./packages/autotalent {};
      cp2104-gpio = pkgs.callPackage ./packages/cp2104-gpio {};
      gregorio = pkgs.callPackage ./packages/gregorio {};
    });

  };

}
