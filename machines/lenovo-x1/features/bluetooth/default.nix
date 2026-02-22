{pkgs, ...}: {
  hardware.bluetooth.enable = true;

  services = {
    blueman.enable = true;
    pulseaudio.package = pkgs.pulseaudioFull;

    # <https://nixos.wiki/wiki/Bluetooth#System-Wide_PulseAudio>:
    pulseaudio.extraConfig = ''
      load-module module-bluetooth-policy
      load-module module-bluetooth-discover
      ## module fails to load with
      ##   module-bluez5-device.c: Failed to get device path from module arguments
      ##   module.c: Failed to load module "module-bluez5-device" (argument: ""): initialization failed.
      # load-module module-bluez5-device
      # load-module module-bluez5-discover
    '';
  };
}
