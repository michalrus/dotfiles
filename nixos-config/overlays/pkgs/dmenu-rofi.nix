self: super:

{

  dmenu-rofi = self.writeScriptBin "dmenu" ''
    #! ${self.stdenv.shell}
    exec ${self.rofi}/bin/rofi -dmenu "$@"
  '';

}
