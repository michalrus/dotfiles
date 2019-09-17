self: super:

{

  hubstaff = let

    version = "1.4.10-848554d6";
    sha256 = "1hwncdzpzawrwswr3ibhxny0aa5k9f8f2qf636bdzqilwhv6342z";

  in super.nixos-unstable.hubstaff.overrideAttrs (drv: {
    name = "hubstaff-${version}";
    src = super.fetchurl {
      url = "https://hubstaff-production.s3.amazonaws.com/downloads/HubstaffClient/Builds/Release/${version}/Hubstaff-${version}.sh";
      inherit sha256;
    };
  });

}
