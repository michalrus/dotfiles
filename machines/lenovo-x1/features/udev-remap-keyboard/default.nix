_: {
  # Find scancodes by running `evtest`.
  # Find devices by running `evemu-describe`.
  # Find ${vendorId} by running `lsusb`.
  # Find modaliases by running `find /sys -name '*modalias' | xargs grep -i ${vendorId}`.

  services.udev.extraHwdb = ''
    evdev:atkbd:dmi:*            # built-in keyboard: match all AT keyboards for now
      KEYBOARD_KEY_3a=leftctrl   # was capslock
      KEYBOARD_KEY_1d=capslock   # was leftctrl
      KEYBOARD_KEY_9d=compose    # was rightctrl

    evdev:input:b0003v062Ap4101*    # my USB keyboard
      KEYBOARD_KEY_70039=leftctrl   # was capslock
      KEYBOARD_KEY_700e0=capslock   # was leftctrl
      KEYBOARD_KEY_700e4=compose    # was rightctrl
  '';
}
