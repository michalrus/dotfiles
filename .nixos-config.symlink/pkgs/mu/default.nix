super: self:

super.mu.overrideDerivation (oldAttrs: {
  patches = [ ./x-smssync.patch ];
})
