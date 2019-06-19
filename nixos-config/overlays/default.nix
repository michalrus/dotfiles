self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "929cc78363e6878e044556bd291382eab37bcbed"
                        "1ghzjk6fq8f2aimp23p45awnfzlcqc20sf7p1xp98myis1sqniwb";
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

  (_: _: { nixos-unstable = nixos-unstable {}; })

  # `services.tor` uses global `pkgs.tor`
  (self: super: { tor = super.nixos-unstable.tor; })

  (self: super: {

    michalrus = composeOverlays [

      (import ./pkgs/hardened-firefox.nix)
      (import ./pkgs/electrum-tor.nix)
      (import ./pkgs/monero-gui-tor.nix)
      (import ./pkgs/wasabi-wallet.nix)

      (import ./pkgs/git-annex-hacks.nix)
      (import ./pkgs/influxdb.nix)
      (import ./pkgs/gnucash.nix) # TODO: move to hledger from this crap
      (import ./pkgs/msmtp-no-security-check.nix)

      # TODO: contribute these:
      (import ./pkgs/gettext-emacs.nix)
      (import ./pkgs/gregorio.nix)
      (import ./pkgs/pms5003.nix)

      # TODO: contributed:

    ] self.michalrus (super.michalrus or super);


    unfree = composeOverlays [

      (_: _: { nixos-unstable = nixos-unstable { allowUnfree = true; }; })

      (self: super: {

        michalrus = composeOverlays [

          (import ./pkgs/steam.nix)

          (import ./pkgs/transcribe.nix)

          (import ./pkgs/hubstaff.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
