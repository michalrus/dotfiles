{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    audacity
    (beets.override { enableCopyArtifacts = true; })
    calibre
    flac
    gimp
    gpac
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
    transcribe
    x264
    youtube-dl

    # for mpd.musicDirectory
    bindfs
  ];

  services.mpd.enable = true;

  fileSystems."${config.services.mpd.musicDirectory}" = {
    device = "${config.users.extraUsers.m.home}/Music";
    fsType = "fuse.bindfs";
    options = [ "force-user=root" "force-group=root" "perms=0000:a+Dr" ];
  };
}
