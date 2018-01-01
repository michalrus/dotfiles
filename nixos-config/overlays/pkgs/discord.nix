self: super:

with (import ../ulib.nix super);


{

  inherit (nixpkgsOf "2de81a55e05a56a8e94d3b5b0876f48c2ff088c5"
               "1vfn2rpmra11pm884xqkiw3q4ym7m9sqj72v3w4wds9za309dd4q"
               { allowUnfree = true; })
      discord;

}
