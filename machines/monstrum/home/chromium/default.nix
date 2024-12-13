{ config, lib, pkgs, ... }:

{
  programs.chromium.enable = true;
  programs.chromium.dictionaries = [ pkgs.hunspellDictsChromium.en_US ];
  programs.chromium.extensions = [
    { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock-origin
    { id = "dneaehbmnbhcippjikoajpoabadpodje"; } # old-reddit-redirect
    { id = "naepdomgkenhinolocfifgehidddafch"; } # browserpass # FIXME: move to ../_shared_/home/password-store
  ];
}
