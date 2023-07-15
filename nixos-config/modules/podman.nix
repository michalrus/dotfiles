{ config, lib, pkgs, ... }:

lib.mkIf config.virtualisation.podman.enable {

  virtualisation.podman.dockerCompat = true;

  environment.systemPackages = with pkgs; [
    buildah
    conmon
    crun  # needs to be in /run/current-system/sw/bin, or else: many warnings
    fuse-overlayfs
    podman
    slirp4netns
  ];

}
