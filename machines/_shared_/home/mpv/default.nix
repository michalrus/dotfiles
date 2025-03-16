{ config, flake, pkgs, lib, ... }:

let
  yt-dlp = flake.packages.${pkgs.system}.yt-dlp;
in

{
  home.packages = [
    pkgs.mpv
    yt-dlp
  ];

  # Streaming only audio:
  home.shellAliases.mpva = "mpv --ytdl-format=bestaudio";

  home.file.".config/mpv/input.conf".text = ''
    MBTN_LEFT  cycle pause

    WHEEL_DOWN add volume -2
    WHEEL_UP   add volume  2
    UP         add volume  2
    DOWN       add volume -2

    LEFT        seek  -5
    RIGHT       seek   5
    SHIFT+LEFT  seek -60
    SHIFT+RIGHT seek  60

    SHIFT+z  add sub-font-size -1
    SHIFT+x  add sub-font-size +1
    CTRL+z   add sub-pos +1
    CTRL+x   add sub-pos -1

    # By default it just toggles them on and off, but let's cycle, since one of the items is empty:
    v cycle sid

    # <https://www.miejski.pl/slowo-Filmy+z+%C5%BC%C3%B3%C5%82tymi+napisami>
    SHIFT+s cycle-values sub-color "#ffff00" "#ffffff"
  '';

  home.file.".config/mpv/mpv.conf".text = ''
    volume-max=400.0
    audio-channels=stereo
    osd-fractions=yes
    osd-duration=1500
    save-position-on-quit=yes
    sub-font-size=44
    sub-codepage=cp1250
    sub-auto=all
    sub-margin-y=0
    sub-pos=98
    script-opts=ytdl_hook-ytdl_path=${lib.getExe yt-dlp}

    # Smooth stream start without stuttering:
    cache-pause-initial=yes
    cache-pause-wait=15
  '';

  # For CLI completion:
  home.shellAliases.youtube-dl = "yt-dlp";

  home.file.".config/yt-dlp/config".text = ''
    --embed-metadata
    --remux-video mp4
  '';
}
