{lib, ...}:
with lib; rec {
  subnet = "10.77.7";

  subnet-chwalecice = "10.77.4";

  tld = "home";

  ccd = {
    michalrus = {ip = "10";};
    elzbietarus = {ip = "11";};
    robertrus-acer = {ip = "12";};
    robertrus-np300e5a = {ip = "13";};
    mikolajrus = {ip = "14";};
    robertrus-asus-1225c = {ip = "15";};
    mikolajrus-rpi = {ip = "16";};
    router-chwalecice = {
      ip = "40";
      extra = "iroute ${subnet-chwalecice}.0 255.255.255.0";
    };
  };

  extraHosts = ''
    ${subnet}.1 router.${tld}
    ${concatStringsSep "\n" (mapAttrsToList (common: v: ''
        ${subnet}.${v.ip} ${common}.${tld}
      '')
      ccd)}
  '';
}
