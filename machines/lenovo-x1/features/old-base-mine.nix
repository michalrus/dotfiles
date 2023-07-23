{ flake, config, lib, pkgs, ... }:

let
  unfree-23_05 = import flake.inputs.nixpkgs { inherit (pkgs) system; config.allowUnfree = true; };
in

{
  networking.networkmanager = {
    enable = true;
    dhcp = "dhclient"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
  };

  programs.ssh.startAgent = false;  # using gpg-agent as ssh-agent

  hardware.pulseaudio = {
    enable = true;
    systemWide = true; # Running multiple concurrent user sessions on different TTYs, I want their audio mixed.
    support32Bit = true;
  };

  # For system-wide PulseAudio: <https://github.com/NixOS/nixpkgs/issues/114399>
  system.activationScripts.fix-pulse-permissions = ''
    chmod 755 /run/pulse
  '';

  systemd.extraConfig = ''
    DefaultCPUAccounting=yes
    DefaultBlockIOAccounting=yes
    DefaultMemoryAccounting=yes
    DefaultTasksAccounting=yes
    DefaultIPAccounting=yes
  '';

  services = {
    logind.lidSwitch = "suspend";
    logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    # No global X11! See <./modules/no-display-manager/i3.nix>
    xserver.enable = lib.mkForce false;
  };
}
