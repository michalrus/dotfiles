{
  lib,
  writeShellApplication,
  findutils,
  git,
  statix,
  deadnix,
  nil,
  nixf,
  jq,
  gnused,
  getopt,
  coreutils,
}:
writeShellApplication {
  name = "nixlint";
  runtimeInputs = [
    findutils
    git
    statix
    deadnix
    nil
    nixf
    jq
    gnused
    getopt
    coreutils
  ];
  text = builtins.readFile ./wrapper.sh;
  derivationArgs.meta.description = "Run Nix linters on files or directories";
  derivationArgs.meta.platforms = lib.platforms.linux;
}
