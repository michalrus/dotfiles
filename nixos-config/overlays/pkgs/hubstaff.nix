self: super:

{

  hubstaff = let

    version = "1.4.3-b4b3cb24";
    sha256 = "0wy8pn6m5pxiv1lgilni9z8hc62j72gfrrbj4yhmxph0jf1afrv9";

  in super.nixos-unstable.hubstaff.overrideAttrs (drv: {
    name = "hubstaff-${version}";
    src = super.fetchurl {
      url = "https://hubstaff-production.s3.amazonaws.com/downloads/HubstaffClient/Builds/Release/${version}/Hubstaff-${version}.sh";
      inherit sha256;
    };
  });

}
