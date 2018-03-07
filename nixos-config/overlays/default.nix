self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "831ef4756e372bfff77332713ae319daa3a42742"
                        "1rbfgfp9y2wqn1k0q00363hrb6dc0jbqm7nmnnmi9az3sw55q0rv";
        nixpkgs = (import src { inherit config; });
    in nixpkgs // {
      preventGC = nixpkgs.writeTextDir "prevent-ifd-gc" (toString [ src ]);
    };

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

  (self: super: {
    nixos-unstable = composeOverlays [
      (import ./pkgs/haskell-ide-engine.nix)
    ] self.nixos-unstable (super.nixos-unstable or (nixos-unstable {}));
  })

  (self: super: {

    michalrus = composeOverlays [

      (import ./pkgs/git-annex-hacks.nix)
      (import ./pkgs/i3.nix)
      (import ./pkgs/influxdb.nix)
      (import ./pkgs/leksah.nix)
      (import ./pkgs/tcp-broadcast.nix)
      (import ./pkgs/gnucash.nix) # TODO: move to hledger from this crap

      # TODO: contribute these:
      (import ./pkgs/gettext-emacs.nix)
      (import ./pkgs/gregorio.nix)
      (import ./pkgs/lemonbar-xft.nix)
      (import ./pkgs/pms5003.nix)

      # TODO: contributed:

      (fromNixpkgs "arpoison" "075b01b35513853a57006ecda04ea981158a869e"
         "05gyim4b309fkv6iqy1dh4lz6v747v0z3p68nc8ns34q8ng5vdgk" {})

    ] self.michalrus (super.michalrus or super);


    unfree = composeOverlays [

      (_: _: { nixos-unstable = nixos-unstable { allowUnfree = true; }; })

      (self: super: {

        michalrus = composeOverlays [

          (import ./pkgs/transcribe.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
