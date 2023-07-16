* With this module, you can run multiple concurrent X11 and Wayland sessions on different TTYs (while with default NixOS configuration, only a single graphical session can be run at a time).

* You can even run two concurrent X11 sessions as a single user.

* All processes (including Wayland and X11 server) run with normal user privileges (unlike with default NixOS configuration, where `X` is run as `root`).

* Just log in into a text console TTY, and type in one of your configured launchers, e.g. `i3`, `i3-low-dpi`, or `sway` etc.
