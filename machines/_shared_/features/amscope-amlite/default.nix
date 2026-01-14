{ flake, config, lib, pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    v4l-utils
    flake.packages.${pkgs.stdenv.hostPlatform.system}.amscope-amlite
  ];

  # FIXME: add a v4l2loopback from libtoupcam.so for other apps – but what about the control knobs?
  # FIXME: not 0666
  services.udev.extraRules = ''
    SUBSYSTEM=="usb", ATTR{idVendor}=="0547", ATTR{idProduct}=="3018", MODE="0666"
    SUBSYSTEM=="usb", ATTR{idVendor}=="0547", ATTR{idProduct}=="3b14", MODE="0666"
  '';
}
