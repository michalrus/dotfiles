{ config, flake, pkgs, lib, ... }:

{
  home.packages = [ pkgs.mpv ];

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
  '';
}
