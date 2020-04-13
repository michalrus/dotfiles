{ config, lib, pkgs, ... }:

{

  # Find scancodes by running `evtest`.

  services.udev.extraHwdb = ''
    evdev:atkbd:dmi:*            # built-in keyboard: match all AT keyboards for now
      KEYBOARD_KEY_3a=leftctrl   # 3a was capslock
      KEYBOARD_KEY_1d=capslock   # 1d was leftctrl
      KEYBOARD_KEY_9d=compose    # 9d was rightctrl
  '';

}
