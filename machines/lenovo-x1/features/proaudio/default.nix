{ flake, config, lib, pkgs, ... }:

{

  # TODO: don’t add these globally…?

  environment.profileRelativeEnvVars = {
    DSSI_PATH   = ["/lib/dssi"];
    LADSPA_PATH = ["/lib/ladspa"];
    LV2_PATH    = ["/lib/lv2"];
    LXVST_PATH  = ["/lib/lxvst"];
    VST_PATH    = ["/lib/vst"];
  };

  # TODO: don’t add these globally…?

  environment.systemPackages = with pkgs; [

    ardour
    distrho-ports
    calf
    x42-plugins
    x42-avldrums
    x42-gmsynth
    flake.packages.${pkgs.stdenv.hostPlatform.system}.autotalent
    flake.packages.${pkgs.stdenv.hostPlatform.system}.talentedhack
    surge
    surge-XT
    flake.packages.${pkgs.stdenv.hostPlatform.system}.vocproc
    flake.packages.${pkgs.stdenv.hostPlatform.system}.tap-plugins
    rubberband

  ];

}
