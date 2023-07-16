{ inputs }:

let nixpkgs = inputs.nixpkgs-lenovo-x1; in

nixpkgs.lib.nixosSystem {
  system = "x86_64-linux";
  modules = [
    { _module.args = { inherit inputs; }; }
    nixpkgs.nixosModules.notDetected

    inputs.self.nixosModules.somagic-easycap

    ../../nixos-config/machines/desktop/hardware/my-lenovo-x1.nix
  ];
}
