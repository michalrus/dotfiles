{
  config,
  lib,
  pkgs,
  ...
}: {
  hardware.logitech.wireless = {
    enable = true;
    enableGraphical = true; # adds <https://github.com/pwr-Solaar/Solaar>
  };
}
