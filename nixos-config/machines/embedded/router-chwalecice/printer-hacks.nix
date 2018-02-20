{ config, pkgs, lib, ... }:

with lib;

# This is a very weird printer. It won’t respond to ARP requests. We
# have to spoof them on this switch.
#
# For details, see <https://serverfault.com/questions/896767/a-faulty-printer-doesn-t-respond-to-arp-requests-how-to-spoof-it-from-a-linux-r>.

let

  inherit (import ../../michalrus.com/openvpn/common.nix { inherit lib; })
    subnet-chwalecice;

  arp-response-spoof = pkgs.callPackage (

    { stdenv }:

    stdenv.mkDerivation rec {
      name = "arp-response-spoof";
      src = ./arp-response-spoof.c;
      unpackCmd = "mkdir src ; cp $curSrc src";
      buildPhase = "gcc -Wall $src";
      installPhase = ''
        mkdir -p $out/bin
        cp a.out $out/bin/arp-response-spoof
      '';
    }

  ) {};

in {

  # This will respond to ARP requests immediately, in place of the
  # printer.
  systemd.services.printer-arp-response-spoof = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = [ arp-response-spoof ];
    script = "exec arp-response-spoof br0 ${subnet-chwalecice}.5 f4:81:39:86:73:cb";
  };

  # The printer falls asleep and stops responding at all. Then, it
  # takes quite a few requests for it to wake up. Let’s try to keep it
  # artificially alive at all times.
  systemd.services.printer-ping = {
    after = [ "network.target" ];
    wantedBy = [ "multi-user.target" ];
    path = with pkgs; [ iputils ];
    script = "exec ping -q -i 2.0 -n ${subnet-chwalecice}.5";
  };

}
