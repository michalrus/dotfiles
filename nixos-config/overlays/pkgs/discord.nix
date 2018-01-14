self: super:

with (import ../ulib.nix super);


{

  inherit (nixpkgsOf "8753b10808338c52d59dc5f2f74fb9c089ab134c"
               "0ln82yxrb0j6vik49q0hwh4m5j18fnmg5p3nlm25cqnd0xlp63r6"
               { allowUnfree = true; })
      discord;

}
