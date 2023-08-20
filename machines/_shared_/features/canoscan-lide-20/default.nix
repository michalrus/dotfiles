{ flake }:  # XXX: it has to be a separate top-level referencee to break infinite recursion

{ ... }:

{
  disabledModules = [ "services/hardware/sane.nix" ];

  # When investigating, see:
  #   • <https://github.com/NixOS/nixpkgs/issues/224569>
  #   • <https://github.com/NixOS/nixpkgs/pull/225339/files>
  imports = [
    (args@{ config, lib, pkgs, ... }:
      import "${flake.inputs.nixpkgs-2305.outPath}/nixos/modules/services/hardware/sane.nix"
      (args // { pkgs = flake.inputs.nixpkgs-2211.legacyPackages.${pkgs.system}; })
    )
  ];

  config = {
    hardware.sane.enable = true;
  };
}



# works: nixpkgs-22.11: /nix/store/axn5lbli0myr90n6vsrvb6b3kwaahymj-sane-config/etc/sane.d
#
# ❯ sane-config --version
# 1.0.32
#
# ❯ sane-fine-scanner
# found USB scanner (vendor=0x04a9 [Canon], product=0x220d [CanoScan], chip=LM9832/3) at libusb:001:020
#
# ❯ scanimage -L
# device `plustek:libusb:001:020' is a Canon CanoScan N670U/N676U/LiDE20 flatbed scanner



# doesn’t work: nixpkgs-23.05: /nix/store/2m0v06dsnc7crwkf8nnlw9qzx0j1d9ky-sane-config/etc/sane.d
#
# ❯ sane-config --version
# 1.2.1
#
# ❯ sane-find-scanner
# found possible USB scanner (vendor=0x04a9 [Canon], product=0x220d [CanoScan], chip=LM9832/3) at libusb:001:020
#
# ❯ scanimage -L
