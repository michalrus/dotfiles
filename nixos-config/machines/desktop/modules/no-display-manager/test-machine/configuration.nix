{ config, lib, pkgs, ... }:

let pwHash = "$6$gSxL/AmTWQc$QmX/k706GJcC97C3pOH21QeN1Zz8xK89Xw8Ec4HIePP5d8yTzjfQPPPogrc8E3wQMf9f7PfSA7wT3Ou4vc6iE/"; in

{
  imports = [
    <nixpkgs/nixos/modules/virtualisation/qemu-vm.nix>
    ../sway.nix
    ../i3.nix
  ];

  virtualisation.memorySize = 1024;
  virtualisation.qemu.options = [ "-vga virtio" ];

  services.logind.extraConfig  =  ''
    KillUserProcesses=yes
    UserStopDelaySec=0
  '';

  environment.extraInit = ''
    # temporarily:
    [ -e ~/.zshrc ] || touch ~/.zshrc

    # temporarily:
    [ -e ~/.config/sway/config ] || {
      mkdir -p ~/.config/sway && ln -sf ${./sample-sway.conf} ~/.config/sway/config
    }
  '';

  networking.hostName = "nixos"; # Define your hostname.

  networking.useDHCP = true;
  networking.usePredictableInterfaceNames = lib.mkForce true;

  time.timeZone = "Europe/Warsaw";

  environment.systemPackages = with pkgs; [
    wget git htop screen aegisub anki firefox
    # pavucontrol
  ];

  programs = {
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.enableCompletion = true;
  };

  # sound.enable = true;
  # hardware.pulseaudio.enable = true;

  users = {
    mutableUsers = false;
    defaultUserShell = "/run/current-system/sw/bin/zsh";
    users = {
      root.hashedPassword = pwHash;
      m = {
        uid = 1234;
        isNormalUser = true;
        hashedPassword = pwHash;
        extraGroups = [ "wheel" ]; # Enable ‘sudo’ for the user.
      };
      m2 = {
        uid = 4321;
        isNormalUser = true;
        hashedPassword = pwHash;
      };
    };
  };
  security.initialRootPassword = pwHash; # not sure why it doesn’t work without this…

}
