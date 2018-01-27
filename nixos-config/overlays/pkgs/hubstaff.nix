self: super:

with (import ../ulib.nix super);


{

  inherit (nixpkgsOf "cd8d6ef9e48642c1dddcc52b01a42beea3e5ff74"
               "10l25qcpjzmnrr2y1hbqxfga2n2r5dzgp1hl81av8ii596qrjlj8"
               { allowUnfree = true; })
      hubstaff;

}
