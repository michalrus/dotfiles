self: super:

{

  libvirt = super.libvirt.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [ ./libvirt--allow-UEFI-snapshots.patch ];
  });

}
