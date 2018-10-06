self: super:

with (import ./ulib.nix super);

let

  nixos-unstable = config:
    let src = nixpkgsOf "0a7e258012b60cbe530a756f09a4f2516786d370"
                        "1qcnxkqkw7bffyc17mqifcwjfqwbvn0vs0xgxnjvh9w0ssl2s036";
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

  (self: super: {

    michalrus = composeOverlays [

      (import ./pkgs/git-annex-hacks.nix)
      (import ./pkgs/influxdb.nix)
      (import ./pkgs/gnucash.nix) # TODO: move to hledger from this crap
      (import ./pkgs/msmtp-no-security-check.nix)

      (fromNixpkgs "sqlint" "5fb615d990031cbde5048c62622bb8c65c4d6980"
         "18jczh5amldjvzv89rfcv1ccnpks8xw63wrgw6z069a9wz7q5sa8" {})

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

          (import ./pkgs/transcribe.nix)

          (fromNixpkgs "discord" "d72b8700797105e6dc38a7518786c35b1692bc00"
             "01pxwg7rkbfpyfrs9qm6fsafd4d8jlw83hfhbv464xc7kzzrb7l0" { allowUnfree = true; })

          (import ./pkgs/hubstaff.nix)

        ] self.michalrus (super.michalrus or super);

      })

    ] self.unfree (super.unfree or (import <nixpkgs> { config.allowUnfree = true; }));

  })

] self super
