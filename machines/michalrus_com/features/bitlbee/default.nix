{
  config,
  pkgs,
  ...
}: {
  services.bitlbee = {
    enable = true;
    plugins = with pkgs; [bitlbee-facebook];
  };
}
