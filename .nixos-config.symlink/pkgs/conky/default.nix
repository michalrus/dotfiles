super: self:

super.conky.overrideDerivation (oldAttrs: {
  patches = [ ./eval.patch ];
})
