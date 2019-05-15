self: super:

{

  hubstaff = let

    version = "1.4.5-c5b459ea";
    sha256 = "180qglbj175wln0kh8d5czhjvy7z503zxn4w6522hkz4ddz201nz";

  in super.nixos-unstable.hubstaff.overrideAttrs (drv: {
    name = "hubstaff-${version}";
    src = super.fetchurl {
      url = "https://hubstaff-production.s3.amazonaws.com/downloads/HubstaffClient/Builds/Release/${version}/Hubstaff-${version}.sh";
      inherit sha256;
    };
  });

}
