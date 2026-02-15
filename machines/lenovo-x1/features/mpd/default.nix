{
  config,
  pkgs,
  ...
}: {
  environment.systemPackages = with pkgs; [
    (beets.override {enableCopyArtifacts = true;})
    keyfinder-cli
    mpc_cli
    ncmpcpp

    # for mpd.musicDirectory
    bindfs
  ];

  # Let’s have MPD send audio to whoever PA is running as.

  services.pulseaudio = {
    enable = true;
    extraConfig = ''
      load-module module-native-protocol-tcp auth-ip-acl=127.0.0.1
    '';
  };

  services.mpd = {
    enable = true;
    extraConfig = ''
      audio_output {
        type        "pulse"
        name        "MPD Pulse Output"
        server      "127.0.0.1"
      }
    '';
  };

  fileSystems."${config.services.mpd.musicDirectory}" = {
    device = "${config.users.extraUsers.m.home}/Music";
    fsType = "fuse.bindfs";
    options = ["chown-ignore" "chgrp-ignore" "force-user=root" "force-group=root" "perms=0000:a+Dr"];
  };
}
