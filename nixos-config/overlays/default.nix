self: super:

with (import ./ulib.nix super);

let

  nixos-unstable' = config:
    (lib.const (lib.const {
      nixos-unstable =
        nixpkgsOf "1bc288591ea4fe3159b7630dcd2b57733d80a2ff"
          "1d4q92jw42d51s9bn380jayy2rs1v0h1y8kvrbjg3i43f72ck5q5"
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
