self: super:

{

  hubstaff = let

    version = "1.4.4-556fe980";
    sha256 = "0fbi4cqabvqrifs0d2v0961i4a9b5nxcw62pz7bjn3b5c8mr7s62";

  in super.nixos-unstable.hubstaff.overrideAttrs (drv: {
    name = "hubstaff-${version}";
    src = super.fetchurl {
      url = "https://hubstaff-production.s3.amazonaws.com/downloads/HubstaffClient/Builds/Release/${version}/Hubstaff-${version}.sh";
      inherit sha256;
    };
  });

}
