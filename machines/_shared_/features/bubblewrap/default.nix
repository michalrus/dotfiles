{ pkgs, ... }:

{
  security.wrappers = {
    # Low-level unprivileged sandboxing tool, see <https://github.com/containers/bubblewrap>.
    bwrap = {
      owner = "root";
      group = "root";
      source = "${pkgs.bubblewrap}/bin/bwrap";
      setuid = true;
    };
  };
}
