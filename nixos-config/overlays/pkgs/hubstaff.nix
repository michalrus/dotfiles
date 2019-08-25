self: super:

{

  hubstaff = let

    version = "1.4.9-86828309";
    sha256 = "0p9b7s2damzxmbrm8m97bj06g0faslbjw51dmxq8icz6ldbqsspx";

  in super.nixos-unstable.hubstaff.overrideAttrs (drv: {
    name = "hubstaff-${version}";
    src = super.fetchurl {
      url = "https://hubstaff-production.s3.amazonaws.com/downloads/HubstaffClient/Builds/Release/${version}/Hubstaff-${version}.sh";
      inherit sha256;
    };
  });

}
