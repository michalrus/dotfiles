{ config, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    audacity
    (beets.override { enableCopyArtifacts = true; })
    calibre
    flac
    gimp
    keyfinder-cli
    lame
    lilypond
    mpc_cli
    mpv
    ncmpcpp
    python34Packages.livestreamer
    rtmpdump
    scantailor
    transcribe

    # for mpd.musicDirectory
    bindfs
  ];

  services.mpd.enable = true;

  fileSystems."${config.services.mpd.musicDirectory}" = {
    device = "${config.users.extraUsers.m.home}/Music";
    fsType = "fuse.bindfs";
    options = [ "ro" "force-user=root" "force-group=root" "perms=444:u+D:g+D:o+D" ];
  };
}
