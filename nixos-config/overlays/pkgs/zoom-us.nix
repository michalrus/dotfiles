self: super:

with (import ../ulib.nix super);

fromNixpkgs "zoom-us"
  "94a0d8ba4fdbb217136c6059b888cd26a6d4886a" # <https://github.com/NixOS/nixpkgs/pull/93374>
  "01rs96rh7lqh2zfm9yajnw2hb80air7h8653h0ayhp3qx8fd2cmv"
  { allowUnfree = true; }
  self super
