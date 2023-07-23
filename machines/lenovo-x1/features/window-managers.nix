{ flake, config, pkgs, lib, ... }:

{

  imports = [ ./old-window-managers.nix ];  # FIXME

  # No global X11 – see ‘flake.nixosModules.no-display-manager’
  services.xserver.enable = lib.mkForce false;

  hardware.pulseaudio = {
    enable = true;
    systemWide = true; # Running multiple concurrent user sessions on different TTYs, I want their audio mixed.
    support32Bit = true;
  };

  # For system-wide PulseAudio: <https://github.com/NixOS/nixpkgs/issues/114399>
  system.activationScripts.fix-pulse-permissions = ''
    chmod 755 /run/pulse
  '';

}
