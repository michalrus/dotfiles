{ config, flake, pkgs, lib, ... }:

{

  home.packages = [ pkgs.screen ];

  home.file.".screenrc".text = ''
    vbell off
    autodetach on
    startup_message off
    bell_msg ""

    altscreen off
    defscrollback 100000

    bind - resize -3
    bind = resize +3
    bind + resize =

    # hack: %s is added at the end (not shown) to refresh load every second
    caption always " %H: %2n: %t %-15=%l %s"
  '';

}
