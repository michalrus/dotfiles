{ pkgs, ... }:

{
  environment.systemPackages = with pkgs; [
    jmtpfs
    libmtp
  ];

  #services.udev.packages = [ pkgs.android-udev-rules ]; # already built-in
}
