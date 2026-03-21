{pkgs, ...}: {
  services.pipewire = {
    enable = true;
    pulse.enable = true;

    # Run a single system-wide PipeWire + WirePlumber instance so that multiple
    # graphical sessions (on separate TTYs) don't each spawn their own
    # WirePlumber and fight over ALSA card profiles.
    systemWide = true;
  };

  # XXX: Users with access to audio need to be added like:
  #users.groups.pipewire.members = ["m" "mw"];

  environment.sessionVariables = {
    # The NixOS module does not set these automatically; without them clients
    # look in $XDG_RUNTIME_DIR and miss the system-wide socket.
    PIPEWIRE_REMOTE = "/run/pipewire/pipewire-0";
    PULSE_SERVER = "unix:/run/pulse/native";
  };

  environment.systemPackages = with pkgs; [
    pavucontrol
    pulsemixer
    alsa-utils
  ];
}
