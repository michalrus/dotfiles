{ self, config, lib, pkgs, ... }:

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
    distrho
    calf
    x42-plugins
    x42-avldrums
    x42-gmsynth
    self.packages.${pkgs.system}.autotalent
    self.packages.${pkgs.system}.talentedhack
    surge
    surge-XT
    self.packages.${pkgs.system}.vocproc
    self.packages.${pkgs.system}.tap-plugins
    rubberband

  ];

}
