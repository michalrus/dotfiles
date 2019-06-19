self: super:

{

  monero-gui-tor = super.monero-gui.overrideAttrs (drv: {
    nativeBuildInputs = (drv.nativeBuildInputs or []) ++ [ self.makeWrapper ];
    postInstall = (drv.postInstall or "") + ''
      wrapProgram $out/bin/monero-wallet-gui

      sed 's,0",\0 torsocks,' -i $out/bin/monero-wallet-gui

      grep -q torsocks $out/bin/monero-wallet-gui
    '';
  });

}
