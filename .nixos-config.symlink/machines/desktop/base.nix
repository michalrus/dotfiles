{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../pkgs
    ../common.nix
  ];

  powerManagement.cpuFreqGovernor = lib.mkDefault "performance";

  networking = {
    hostName = lib.mkDefault "nixos";
    extraHosts = "127.0.0.1 ${config.networking.hostName}";
    nameservers = lib.mkDefault [ "8.8.8.8" "8.8.4.4" ];
    firewall.nonetGroup.enable = lib.mkDefault true;
  };

  hardware = {
    sane.enable = true;
    opengl.driSupport32Bit = true; # for Wine
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

  nixpkgs.config = {
    allowBroken = true;
  };

  services = {
    printing = {
      enable = true;
      gutenprint = true;
    };

    logkeys = {
      enable = true;
      keymap = lib.mkDefault "pl";
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    xserver = {
      enable = true;
      layout = lib.mkDefault "pl";
      xkbOptions = "ctrl:nocaps,compose:caps";
      synaptics = {
        enable = true;
        twoFingerScroll = true;
        tapButtons = true;
        fingersMap = [1 3 2];
      };
    };
  };

  fonts = {
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      terminus_font
      unifont
      unifont_upper
    ];
  };

  users.users.root = {
    linger = true;
    openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCbtBIiVRih4vlbX8ciQELf9wST0tYIygNfPxNjtm1IElpOKVT+j62hPui+d0VELKuxJcyo3tY9nf2zUYUG3PG7IWjyiHi6FyOasUQLzJrXBRj5dNsPr+SYXAyL1jsTbvbfiIUkfPAPuv5Tf/tg/lAdTriTy73V5sN7vtX+MH2k8n4agE6fhj2FAhiSwI4MAZJmIsNB2X+1GZVLZlggpN7tkkfjFWE5nCvlR+/lA6e0wl9ZCzTas112fTTBUk64wd1U7vlv1+nr7YgVAqyAQR/w7VCe0z3hrwIwxCOdW3nN19dW2gCQ7gKrZbDfaU3/OqURTNq9zwdET/mNM7unF4sX" ];
  };

  system.autoUpgrade = {
    enable = lib.mkDefault true;
    dates = "1:30";
  };
}
