{

  # Import all files in `~/.nixos-config/local/` matching `*.nix`.

  imports =
    builtins.map
      (n: ./. + ("/" + n))
      (builtins.filter
        (n: builtins.substring ((builtins.stringLength n) - 4) 4 n == ".nix")
        (builtins.attrNames (builtins.readDir ./. )));

}
