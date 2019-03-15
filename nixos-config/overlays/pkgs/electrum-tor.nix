self: super:

{

  electrum-tor = super.electrum.overrideAttrs (drv: {
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ self.makeWrapper ];
    postInstall = (drv.postInstall or "") + ''
      wrapProgram $out/bin/electrum --add-flags '-p socks5:127.0.0.1:9050'
    '';
  });

}
