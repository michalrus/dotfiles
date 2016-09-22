{ config, lib, pkgs, ... }:

let

  sdk = pkgs.androidenv.androidsdk {
    platformVersions = [ "21" "23" ];
    abiVersions = [ "armeabi-v7a" "x86" "x86_64"];
    useGoogleAPIs = true;
    useExtraSupportLibs = true;
  };

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
