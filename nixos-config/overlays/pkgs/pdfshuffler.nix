self: super:

with (import ../ulib.nix super);

fromNixpkgs "pdfshuffler" "327a84749ed48a20736fdf20b9dd4f5723b01912"
  "0fgdcy49w073iiy9i65928219n1fy3w61xxsyqn6d8a72dxpcs3n"
  {}
  self super
