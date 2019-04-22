self: super:

with (import ../ulib.nix super);

fromNixpkgs "steam" "9b1706eb0601f8bdb8a7eedcd5e8bd24a1e3b960"
  "14nawi3xhngryfrp5wrwqalvjq2xvbxwj6jh4w3b5i5ji41ii82a"
  { allowUnfree = true; }
  self super
