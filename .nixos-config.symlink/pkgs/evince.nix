super: self:

super.evince.overrideDerivation (oldAttrs: {
  src = super.evince;
  installPhase = "rm -r lib/mozilla && cp -a . $out";
})
