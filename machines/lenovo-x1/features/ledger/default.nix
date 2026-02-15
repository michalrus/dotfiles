{pkgs, ...}: {
  services.udev.packages = [
    pkgs.ledger-udev-rules

    # Ledger Nano S Plus
    (pkgs.writeTextDir "lib/udev/rules.d/19-ledger-generic.rules" ''
      # The newest Ledger rule (not yet in Nixpkgs):
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0005|5000|5001|5002|5003|5004|5005|5006|5007|5008|5009|500a|500b|500c|500d|500e|500f|5010|5011|5012|5013|5014|5015|5016|5017|5018|5019|501a|501b|501c|501d|501e|501f", TAG+="uaccess", TAG+="udev-acl"
    '')
  ];
}
