self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "2e4aded366914d625a2f31208e8ac8548cb43a7e"
                        "1zcbvzxk1vyk4ngfdyss8mlb3mqp050ygpnwqny0bxbzlqkrc4bh";
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

      # TODO: move to hledger from this crap
      (fromNixpkgs "gnucash26" "360089b3521af0c69a5167870c80851dedf19d76" # nixos-17.03 :/
         "1ag2hfsv29jy0nwlwnrm5w2sby0n1arii7vcc0662hr477f4riaq"
         { allowBroken = true; permittedInsecurePackages = [ "webkitgtk-2.4.11" ]; })

      # TODO: contribute these:
      (import ./pkgs/gettext-emacs.nix)
      (import ./pkgs/gregorio.nix)
      (import ./pkgs/lemonbar-xft.nix)
      (import ./pkgs/pms5003.nix)

      # TODO: contributed:

      (fromNixpkgs "watchexec" "1bccb28904ff1c1ea2fb6278fc950ebd5c8aed1d"
         "04i20pwq1cfgqs2ds358yzq9c38ip55mkx35w8nhx44xs6y27g9x" {})

      (fromNixpkgs "catdocx" "0552147a3456662908646c9896c5149788a0982c"
         "15vvcw8nv3z1lrqiphrxc7lrk2sb86yk274gy5pl7j8kbjsvbdni" {})

      (fromNixpkgs "arping" "986ab982b5b59137418ca2ae07fc8ac4fbb62134"
         "1j2zqrvhc7c5j71vxhy3ys596vdc9x9kii6fj9fbrxi100hgzyhn" {})

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
