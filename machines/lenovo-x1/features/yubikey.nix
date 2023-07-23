{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    yubikey-manager
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
  ];

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

}
