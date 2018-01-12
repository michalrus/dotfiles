self: super:

with (import ./ulib.nix super);

let

  nixos-unstable' = config:
    (lib.const (lib.const {
      nixos-unstable =
        nixpkgsOf "f607771d0f5e4fa905afff1c772febd9f3103e1a"
          "1icphqpdcl8akqhfij2pxkfr7wfn86z5sr3jdjh88p9vv1550dx7"
          config;
    }));

in

#
# You can try out any given package by running:
#
#   $ nix-build -E 'with import <nixpkgs> { overlays = [ (import ./pkgs/tcp-broadcast.nix) ]; }; tcp-broadcast'
#
#   $ cd result/
#

composeOverlays [

  # `config.programs.mtr` uses the global definition… 🙄
  (import ./pkgs/mtr.nix)

  (nixos-unstable' {})

  (self: super: {

    michalrus = composeOverlays [

      (import ./pkgs/git-annex-hacks.nix)
      (import ./pkgs/i3.nix)
      (import ./pkgs/influxdb.nix)
      (import ./pkgs/leksah.nix)
      (import ./pkgs/steeloverseer.nix)
      (import ./pkgs/tcp-broadcast.nix)

      # TODO: contribute these:
      (import ./pkgs/catdocx.nix)
      (import ./pkgs/gettext-emacs.nix)
      (import ./pkgs/gregorio.nix)
      (import ./pkgs/lemonbar-xft.nix)
      (import ./pkgs/pms5003.nix)

    ] self.michalrus (super.michalrus or super);


    unfree = composeOverlays [

      (nixos-unstable' { allowUnfree = true; })

      (self: super: {

        michalrus = composeOverlays [

          (import ./pkgs/discord.nix)
          (import ./pkgs/transcribe.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
