self: super:

{

  transmission = super.transmission.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [ ./transmission--no-deleteLocalData.patch ];
  });

}
