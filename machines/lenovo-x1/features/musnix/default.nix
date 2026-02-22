{pkgs, ...}: let
  # TODO: move to flake inputs
  repo = pkgs.fetchFromGitHub {
    owner = "musnix";
    repo = "musnix";
    rev = "7207f25dc03c55488cc495f3db5641c3c0ba5f96";
    sha256 = "135jlcjm1s6lj9alhqllms6d7qsx43x609lyjck5waif8s7a3fz4";
  };
in {
  imports = ["${repo}"];

  musnix = {
    enable = true;
    # soundcardPciId = ""; # TODO: Set me. // Maybe this should go to `hardware-configuration.nix`?
    kernel = {
      optimize = true;
      realtime = true;
      packages = pkgs.linuxPackages_4_4_rt;
    };
    rtirq.enable = true;
    # rtirq.highList = [ ??? ]; # TODO: Set me.
  };

  environment = {
    # Ardour4’s GUI eats 90%+ CPU if this one is not set.
    variables."ARDOUR_IMAGE_SURFACE" = "1";

    systemPackages = with pkgs; [
      (writeTextDir "prevent-ifd-gc" (toString [repo]))
      a2jmidid
      aeolus
      airwave
      ardour
      artyFX
      calf
      distrho-ports
      eq10q
      guitarix
      helm
      jack2Full
      mda_lv2
      qjackctl
      setbfree
      squishyball
      x42-plugins
    ];

    profileRelativeEnvVars = {
      DSSI_PATH = ["/lib/dssi"];
      LADSPA_PATH = ["/lib/ladspa"];
      LV2_PATH = ["/lib/lv2"];
      LXVST_PATH = ["/lib/lxvst"];
      VST_PATH = ["/lib/vst"];
    };
  };

  services.pulseaudio.package = pkgs.pulseaudioFull;

  users.extraUsers.m.extraGroups = ["audio"];
}
