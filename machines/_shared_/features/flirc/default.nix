{
  flake,
  pkgs,
  ...
}: {
  environment.systemPackages = [
    flake.packages.${pkgs.stdenv.hostPlatform.system}.flirc
  ];

  services.udev.extraRules = ''
    # Flirc bootloader
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTR{idVendor}=="20a0", ATTR{idProduct}=="0000", MODE="0666"
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTR{idVendor}=="20a0", ATTR{idProduct}=="0002", MODE="0666"
    SUBSYSTEM=="hidraw", ATTRS{idVendor}=="20a0", ATTRS{idProduct}=="0005", MODE="0666"
    # Flirc application
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTR{idVendor}=="20a0", ATTR{idProduct}=="0001", MODE="0666"
    SUBSYSTEM=="usb", ENV{DEVTYPE}=="usb_device", ATTR{idVendor}=="20a0", ATTR{idProduct}=="0004", MODE="0666"
    SUBSYSTEM=="hidraw", ATTRS{idVendor}=="20a0", ATTRS{idProduct}=="0006", MODE="0666"
  '';

  # Flirc sends HID Consumer 0xCD `KEY_PLAYPAUSE` which XKB by default maps to
  # `XF86AudioPlay` – same keysym as plain `KEY_PLAYCD`. Unfortunately
  # freedesktop.org lacks `XF86AudioPlayPause`, so we remap to a distinct key
  # (`XF86Launch1`) so Hyprland can bind it to `playerctl play-pause` without
  # conflicting with devices like headphones that send `KEY_PLAYCD` and
  # `KEY_PAUSECD` separately.
  services.udev.extraHwdb = ''
    evdev:input:b0003v20A0p0006*
      KEYBOARD_KEY_000c00cd=prog1
  '';
}
