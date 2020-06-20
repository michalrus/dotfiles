{ config, lib, pkgs, ... }:

{

  # TODO: re-enable after <https://github.com/containers/libpod/pull/5550> <https://github.com/containers/crun/commit/a171c3bb7316c29b0e2d207327e0e39b8243b538>
  security.hideProcessInformation = lib.mkForce false;

  # TODO: use `virtualisation.podman.enable = true;` on >20.03

  environment.systemPackages = with pkgs; [

    podman runc conmon slirp4netns fuse-overlayfs

    # TODO: use `virtualisation.podman.dockerCompat = true;` on >20.03
    (runCommand "docker-compat" {} ''
      mkdir -p $out/bin
      ln -s ${podman}/bin/podman $out/bin/docker
    '')

  ];

  environment.etc."containers/policy.json" = {
    mode="0644";
    text=''
      {
        "default": [
          {
            "type": "insecureAcceptAnything"
          }
        ],
        "transports":
          {
            "docker-daemon":
              {
                "": [{"type":"insecureAcceptAnything"}]
              }
          }
      }
    '';
  };

  environment.etc."containers/registries.conf" = {
    mode="0644";
    text=''
      [registries.search]
      registries = ['docker.io', 'quay.io']
    '';
  };

}
