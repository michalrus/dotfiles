{ flake, config, lib, pkgs, ... }:

let
  unfree-unstable = import flake.inputs.nixpkgs-unstable.outPath {
    inherit (pkgs) system; config.allowUnfree = true;
  };
in

{
  # FIXME: No custom ~/.XCompose definitions in Chromium on native Wayland
  # FIXME: <https://issues.chromium.org/issues/40272818>
  # FIXME: <https://fcitx-im.org/wiki/Using_Fcitx_5_on_Wayland#Chromium_.2F_Electron>

  programs.chromium = {
    enable = true;
    package = unfree-unstable.chromium.override {
      enableWideVine = true;
    };
    commandLineArgs = [
      "--ozone-platform-hint=auto" # native Wayland
    ];
    dictionaries = [ pkgs.hunspellDictsChromium.en_US ];
    extensions = [
      { id = "cjpalhdlnbpafiamejdnhcphjbkeiagm"; } # ublock-origin
      { id = "dneaehbmnbhcippjikoajpoabadpodje"; } # old-reddit-redirect
      { id = "naepdomgkenhinolocfifgehidddafch"; } # browserpass # FIXME: move to ../_shared_/home/password-store
    ];
  };
}
