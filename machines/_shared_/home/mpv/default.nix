{ config, flake, pkgs, lib, ... }:

{

  home.packages = [ pkgs.mpv ];

  home.file.".config/mpv/input.conf".text = ''
    SHIFT+z add sub-font-size -1
    SHIFT+x add sub-font-size +1

    CTRL+z add sub-pos +1
    CTRL+x add sub-pos -1

    SHIFT+s cycle-values sub-color "#ffff00" "#ffffff"
  '';

  home.file.".config/mpv/mpv.conf".text = ''
    volume-max=400.0
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
