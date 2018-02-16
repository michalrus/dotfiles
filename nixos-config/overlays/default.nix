self: super:

with (import ./ulib.nix super);

let

  nixos-unstable' = config:
    (lib.const (lib.const rec {

      nixos-unstable =
        let src = nixpkgsOf "2e4aded366914d625a2f31208e8ac8548cb43a7e"
                            "1zcbvzxk1vyk4ngfdyss8mlb3mqp050ygpnwqny0bxbzlqkrc4bh";
            nixpkgs = (import src { inherit config; });
        in nixpkgs // {
          preventGC = nixpkgs.writeTextDir "prevent-ifd-gc" (toString [ src ]);
        };
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
      (import ./pkgs/gnucash-old.nix) # TODO: move to hledger from this crap

      # TODO: contribute these:
      (import ./pkgs/arping.nix)
      (import ./pkgs/arpoison.nix)
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
          (import ./pkgs/hubstaff.nix)
          (import ./pkgs/transcribe.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
