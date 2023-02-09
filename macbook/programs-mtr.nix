{ pkgs, lib, ... }:

let

  mtrWrapped = pkgs.mtr.overrideAttrs (drv: {
    postFixup = (drv.postFixup or "") + ''
      mv $out/bin/mtr-packet $out/bin/.mtr-packet-wrapped
      cat >$out/bin/mtr-packet <<EOF
      #!/bin/sh
      exec sudo -n $out/bin/.mtr-packet-wrapped
      EOF
      chmod +x $out/bin/mtr-packet
    '';
  });

in

{

  environment.etc."sudoers.d/80-mtr-packet".text = ''
    ALL ALL = (root) NOPASSWD: ${mtrWrapped}/bin/.mtr-packet-wrapped
  '';

  environment.systemPackages = with pkgs; [
    mtrWrapped
  ];

}
