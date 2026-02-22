_: {
  boot.kernel.sysctl."vm.swappiness" = 60; # Let’s try the default 60 with `zramSwap`.
  swapDevices = [];
  zramSwap = {
    enable = true;
    memoryPercent = 50;
    algorithm = "zstd";
  };
}
