self: super:

with (import ../ulib.nix super);

let base =
      (fromNixpkgs "gnucash26" "327a84749ed48a20736fdf20b9dd4f5723b01912" # nixos-unstable
        "0fgdcy49w073iiy9i65928219n1fy3w61xxsyqn6d8a72dxpcs3n"
        { permittedInsecurePackages = [ "webkitgtk-2.4.11" ]; }
        self super)
        .gnucash26;

in {

  gnucash26 = base.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [ ./gnucash-crypto.patch ];
  });

}
