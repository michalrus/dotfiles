{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    yubikey-manager
    #yubikey-manager-qt # end of life
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-flutter
  ];

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

}
