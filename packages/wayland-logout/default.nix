{
  lib,
  writeShellApplication,
  fuzzel,
}:
writeShellApplication {
  name = "wayland-logout";
  runtimeInputs = [fuzzel];
  text = ''
    set -euo pipefail

    chosen=$(<<<'Lock session
    Log out
    Suspend
    Switch to console (VT) 1
    Switch to console (VT) 2
    Switch to console (VT) 3
    Switch to console (VT) 4
    Switch to console (VT) 5
    Switch to console (VT) 6
    Reboot
    Hibernate
    Power off' fuzzel --dmenu --index)

    case "$chosen" in
    0)  sudo /run/current-system/sw/bin/loginctl lock-sessions ;;
    1)  hyprctl dispatch exit ;;
    2)  systemctl suspend -i ;;
    3)  sudo /run/current-system/sw/bin/loginctl lock-sessions && sleep 1 && sudo chvt 1 ;;
    4)  sudo /run/current-system/sw/bin/loginctl lock-sessions && sleep 1 && sudo chvt 2 ;;
    5)  sudo /run/current-system/sw/bin/loginctl lock-sessions && sleep 1 && sudo chvt 3 ;;
    6)  sudo /run/current-system/sw/bin/loginctl lock-sessions && sleep 1 && sudo chvt 4 ;;
    7)  sudo /run/current-system/sw/bin/loginctl lock-sessions && sleep 1 && sudo chvt 5 ;;
    8)  sudo /run/current-system/sw/bin/loginctl lock-sessions && sleep 1 && sudo chvt 6 ;;
    9)  systemctl reboot ;;
    10) systemctl hibernate -i ;;
    11) systemctl poweroff ;;
    esac
  '';
  derivationArgs.meta.description = "A simple logout menu for Wayland";
  derivationArgs.meta.platforms = lib.platforms.linux;
}
