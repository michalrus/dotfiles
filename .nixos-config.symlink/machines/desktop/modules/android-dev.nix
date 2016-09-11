{ config, lib, pkgs, ... }:

let

  sdk = pkgs.androidsdk;
  ndk = pkgs.androidndk;

in

{
  imports = [
    ./android.nix
  ];

  environment.systemPackages = [ sdk ndk ];

  environment.variables."ANDROID_HOME"     = "${sdk}/libexec";
  environment.variables."ANDROID_NDK_HOME" = "${ndk}/libexec/${ndk.name}";
}
