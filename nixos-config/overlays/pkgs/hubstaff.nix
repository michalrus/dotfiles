self: super:

{

  hubstaff = let

    version = "1.4.6-4aba8ab5";
    sha256 = "14js4d7wazn5r5p9n3iic9kwqrinm079jj1k7r00v684jrgq62fc";

  in super.nixos-unstable.hubstaff.overrideAttrs (drv: {
    name = "hubstaff-${version}";
    src = super.fetchurl {
      url = "https://hubstaff-production.s3.amazonaws.com/downloads/HubstaffClient/Builds/Release/${version}/Hubstaff-${version}.sh";
      inherit sha256;
    };
  });

}
