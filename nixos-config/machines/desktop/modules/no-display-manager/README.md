* In this configuration, you can run multiple concurrent X11 and Wayland sessions on different TTYs (while with default NixOS configuration, only a single graphical session can be run at a time).

* You can even run two concurrent `i3` sessions as a single user.

* All processes (including Wayland and X11 server) run with normal user privileges (unlike with default NixOS configuration, where `X` is run as `root`).

* Just log in into a virtual console TTY, and type either `i3` or `sway`.
