{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    aegisub
    audacity
    (beets.override { enableCopyArtifacts = true; })
    calibre
    cdparanoia
    devede
    flac
    gimp
    gpac
    handbrake
    inkscape
    keyfinder-cli
    lame
    libjpeg
    lilypond
    mpc_cli
    mpv
    ncmpcpp
    python34Packages.livestreamer
    rtmpdump
    scantailor
    shntool
    timidity
    transcribe
    x264
    youtube-dl

    # for mpd.musicDirectory
    bindfs
  ];

  # Let’s have MPD send audio to whoever PA is running as.

  hardware.pulseaudio = {
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
    options = [ "chown-ignore" "chgrp-ignore" "force-user=root" "force-group=root" "perms=0000:a+Dr" ];
  };
}
