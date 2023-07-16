{ ... }:

{

  ## Allow unsafe internal UEFI snapshots in libvirt globally:
  nixpkgs.overlays = [
    (_: super: {
      libvirt = super.libvirt.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ [ ./libvirt--allow-UEFI-snapshots.patch ];
      });
    })
  ];

  virtualisation.libvirtd.enable = true; # QEMU/KVM
  virtualisation.spiceUSBRedirection.enable = true;


}
