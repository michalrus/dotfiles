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
    hibernate-on-low-battery
    lock-vts
    malicious-hosts
    somagic-easycap

    torified-users
    ./features/tor.nix

    ./features/android.nix
    ./features/libvirt.nix
    ./features/podman.nix

  ]);
}
