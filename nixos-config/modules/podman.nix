{ config, lib, pkgs, ... }:

lib.mkIf config.virtualisation.podman.enable {

  virtualisation.podman.dockerCompat = true;

  # TODO: re-enable after <https://github.com/containers/libpod/pull/5550>
  #       <https://github.com/containers/crun/commit/a171c3bb7316c29b0e2d207327e0e39b8243b538>
  security.hideProcessInformation = lib.mkForce false;

  environment.systemPackages = with pkgs; [
    buildah
    conmon
    crun  # needs to be in /run/current-system/sw/bin, or else: many warnings
    fuse-overlayfs
    podman
    slirp4netns
  ];

}
