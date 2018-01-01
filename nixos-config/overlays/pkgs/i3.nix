self: super:

{

  i3 = super.i3.overrideAttrs (oldAttrs: {

    patches = [ (self.fetchurl {
      url = "https://patch-diff.githubusercontent.com/raw/i3/i3/pull/2953.diff";
      sha256 = "05f3xibr9yx6hm1hzryvagfryn0mlh1vrx182frszxmcygc4kl3z";
    }) ];

  });

}
