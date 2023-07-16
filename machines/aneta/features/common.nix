{

  addressing = let p = "10.77.2"; in rec {
    router        = "${p}.1";
    prefix        = "${p}.0";
    prefixLength  = 24;
    netmask       = "255.255.255.0";
    subnet        = "${prefix}/${toString prefixLength}";
    broadcast     = "${p}.255";
    dhcpRangeLow  = "${p}.100";
    dhcpRangeHigh = "${p}.250";

    rpiKrzysiek   = "${p}.51";
    printer       = "${p}.5";
  };

}
