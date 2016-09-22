super: self:

super.android-udev-rules.overrideDerivation (oldAttrs: {
  patches = [ ./MT6580.patch  ];
})
