{ pkgs, ... }:

{

  environment.systemPackages = with pkgs; [
    yubikey-manager
    #yubikey-manager-qt # end of life
    yubikey-personalization
    #yubikey-personalization-gui # End of life. Consider using 'yubioath-flutter' instead.
    yubioath-flutter
  ];

  services.udev.packages = with pkgs; [
    yubikey-personalization
  ];

}
