super: self:

let
  map = {
    pl = (builtins.toFile "logkeys-pl.map" (builtins.readFile ./pl.map));
  };
in

super.logkeys.overrideDerivation (oldAttrs: {

  postInstall = ''
    mkdir -p "$out"/share/logkeys/
    cp -r keymaps/. "$out"/share/logkeys/
    ln -s "${map.pl}" "$out"/share/logkeys/pl.map
    '';

  systemdUnit = {
    description = "Log all keys pressed on all keyboards";
    serviceConfig.Type = "forking";
    wantedBy = [ "multi-user.target" ];
    path = with self; [ logkeys ];
    script = ''
      ls /dev/input/by-path | grep kbd | while IFS= read -r inp ; do
        rinp="$(readlink -f "/dev/input/by-path/$inp")"
        logkeys --start --device="$rinp" --output=/var/log/logkeys.log --keymap="${self.logkeys}/share/logkeys/pl.map"
        # why is the following not configurable?!
        rm /var/run/logkeys.pid
      done
      '';
    outPath = "unused";
  };

})
