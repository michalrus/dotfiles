{ pkgs, ... }:

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

  environment.systemPackages = with pkgs; [
    virt-manager # Gtk3 for QEMU/KVM
  ];

  programs.dconf.enable = true; # for virt-manager

  # <https://github.com/kholia/OSX-KVM/blob/master/kvm.conf>
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1 report_ignored_msrs=0
  '';

  users.extraUsers.m.extraGroups = [ "libvirtd" ];
  users.extraUsers.mw.extraGroups = [ "libvirtd" ];

}
