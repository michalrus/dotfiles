self: super:

{

  msmtp-no-security-check = super.msmtp.overrideAttrs (oldAttrs: {
    patches = (oldAttrs.patches or []) ++ [ ./msmtp-no-security-check.patch ];
  });

}
