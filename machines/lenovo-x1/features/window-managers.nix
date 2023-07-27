{ flake, config, pkgs, lib, ... }:

{

  imports = [ ./old-window-managers.nix ];  # FIXME

  services.xserver = {
    enable = lib.mkForce false;  # no global X11 – see ‘flake.nixosModules.no-display-manager’
    layout = lib.mkDefault "pl";
    synaptics = {
      enable = lib.mkDefault true;
      twoFingerScroll = true;
      tapButtons = true;
      fingersMap = [1 3 2];
    };
  };

}
