{...}: {
  networking.firewall.rejectPackets = true; # I want servers to be responsive, even on failures
}
