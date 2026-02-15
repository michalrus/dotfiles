{pkgs, ...}: {
  ## Allow unsafe internal UEFI snapshots in libvirt globally:
  /*
  nixpkgs.overlays = [
    (_: super: {
      libvirt = super.libvirt.overrideAttrs (drv: {
        patches = (drv.patches or []) ++ [ ./libvirt--allow-UEFI-snapshots.patch ];
      });
    })
  ];
  */

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

  users.extraUsers.m.extraGroups = ["libvirtd"];
  users.extraUsers.mw.extraGroups = ["libvirtd"];

  networking.firewall.allowedTCPPorts = [2049]; # NFS v4 server

  services.nfs.server = {
    enable = true;
    exports = ''
      /home/mw/VMs/Shared     127.0.0.1/8(insecure,rw,sync,no_subtree_check,all_squash,anonuid=1337,anongid=100)
    '';
  };

  fileSystems."/var/home/mw/VM-Shared/win10-new" = {
    device = "//192.168.122.241/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=unused"
      "password="
      "vers=2.0"
      "uid=mw"
      "forceuid"
      "gid=users"
      "forcegid"
      "file_mode=0644"
      "dir_mode=0755"
      "nofail"
      "_netdev"
      "x-systemd.automount"
      "x-systemd.mount-timeout=5s"
      "x-systemd.device-timeout=5s"
      "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/win10" = {
    device = "//192.168.122.239/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=unused"
      "password="
      "vers=2.0"
      "uid=mw"
      "forceuid"
      "gid=users"
      "forcegid"
      "file_mode=0644"
      "dir_mode=0755"
      "nofail"
      "_netdev"
      "x-systemd.automount"
      "x-systemd.mount-timeout=5s"
      "x-systemd.device-timeout=5s"
      "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/ubuntu" = {
    device = "//192.168.122.114/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=unused"
      "password="
      "vers=2.0"
      "uid=mw"
      "forceuid"
      "gid=users"
      "forcegid"
      "file_mode=0644"
      "dir_mode=0755"
      "nofail"
      "_netdev"
      "x-systemd.automount"
      "x-systemd.mount-timeout=5s"
      "x-systemd.device-timeout=5s"
      "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/macos11" = {
    device = "//192.168.122.75/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=mw"
      "password=dupa.8"
      "vers=2.0"
      "uid=mw"
      "forceuid"
      "gid=users"
      "forcegid"
      "file_mode=0644"
      "dir_mode=0755"
      "nofail"
      "_netdev"
      "x-systemd.automount"
      "x-systemd.mount-timeout=5s"
      "x-systemd.device-timeout=5s"
      "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/macos11-dev" = {
    device = "//192.168.122.77/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=mw"
      "password=dupa.8"
      "vers=2.0"
      "uid=mw"
      "forceuid"
      "gid=users"
      "forcegid"
      "file_mode=0644"
      "dir_mode=0755"
      "nofail"
      "_netdev"
      "x-systemd.automount"
      "x-systemd.mount-timeout=5s"
      "x-systemd.device-timeout=5s"
      "x-systemd.idle-timeout=2min"
    ];
  };
}
