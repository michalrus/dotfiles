{ config, pkgs, ... }:

{
  imports = [
    ../../modules
    ../../pkgs
    ../../local
    ../../common.nix
    ../../hardware-configuration.nix
    ../../boot.nix
  ];

  nix.useChroot = true;   # move to common.nix when in stable!

  powerManagement = {
    cpuFreqGovernor = "performance";
  };

  networking = {
    hostName = "nixos";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    networkmanager.enable = true;
  };

  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "pl_PL.UTF-8";

  hardware = {
    sane.enable = true;
    sane.extraConfig.pixma = "bjnp://10.0.1.5";
  };

  nixpkgs.config = {
    allowBroken = true;
    packageOverrides = super: let self = super.pkgs; in {
      # By default, gvfs in Xfce has no Samba support. Turn it back on.
      xfce = super.xfce // { gvfs = super.gvfs; };
      # Feh is always added to system PATH, see #17450.
      feh = super.feh.overrideDerivation(oldAttrs: { postInstall = "rm $out/share/applications/feh.desktop"; });
    };
  };

  # Use GTK 2 in LibreOffice. 3 has some menu rendering problems.
  environment.variables."SAL_USE_VCLPLUGIN" = "gtk";

  environment.systemPackages = with pkgs; [
    (runCommand "wrap-slimlock" {} "mkdir -p $out/bin && ln -s ${pkgs.slim}/bin/slimlock $out/bin/slock")
    awf-gtk
    chromium
    cool-retro-term
    evince
    galculator
    gimp
    gnome3.aisleriot
    gnome3.file-roller # for thunar-archive-plugin
    jmtpfs
    libmtp
    libnotify
    libreoffice
    mpv
    networkmanagerapplet
    openjdk8 # Thunderbird seems to need it for some LibreOffice extension? And I *think* it crashes without it. Could not replicate, though.
    pinentry
    python34Packages.livestreamer
    rtmpdump
    simple-scan
    thunderbird
    transmission_gtk
    unclutter
    xarchiver
    xclip
    xdotool
    xorg.xbacklight
    xorg.xev
    xorg.xdpyinfo
    xorg.xmodmap
    xrandr-invert-colors
    xsane
    xsel
  ];

  services = {
    printing = {
      enable = true;
      gutenprint = true;
    };

    logkeys = {
      enable = true;
      keymap = "pl";
    };

    openssh = {
      enable = true;
      passwordAuthentication = false;
      challengeResponseAuthentication = false;
    };

    xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "ctrl:nocaps,compose:caps";
      synaptics = {
        enable = true;
        maxSpeed = "5.0";
        accelFactor = "0.025";
        twoFingerScroll = true;
        tapButtons = true;
        fingersMap = [1 3 2];
      };

      displayManager.auto.enable = true;
      displayManager.auto.user = "elzbieta";
      desktopManager.xterm.enable = false;
      desktopManager.xfce = {
        enable = true;
        thunarPlugins = with pkgs.xfce; [ thunar-archive-plugin ];
      };
    };
  };

  fonts = {
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      corefonts
      terminus_font
      unifont
      unifont_upper
      vistafonts
    ];
  };

  users = {
    users.root = {
      linger = true;
      openssh.authorizedKeys.keys = [ "ssh-rsa AAAAB3NzaC1yc2EAAAADAQABAAABAQCbtBIiVRih4vlbX8ciQELf9wST0tYIygNfPxNjtm1IElpOKVT+j62hPui+d0VELKuxJcyo3tY9nf2zUYUG3PG7IWjyiHi6FyOasUQLzJrXBRj5dNsPr+SYXAyL1jsTbvbfiIUkfPAPuv5Tf/tg/lAdTriTy73V5sN7vtX+MH2k8n4agE6fhj2FAhiSwI4MAZJmIsNB2X+1GZVLZlggpN7tkkfjFWE5nCvlR+/lA6e0wl9ZCzTas112fTTBUk64wd1U7vlv1+nr7YgVAqyAQR/w7VCe0z3hrwIwxCOdW3nN19dW2gCQ7gKrZbDfaU3/OqURTNq9zwdET/mNM7unF4sX" ];
    };
    extraUsers.elzbieta = {
      hashedPassword = "$6$W/KppVZSY$.vf1jfCd6H0tOJwRwmUwJeMSkmg/MyDUlNpx3IRHWjmLpyXyg5quW0VRBX4QwGp00MIT6Nw2nODs.JhleHblz1";
      isNormalUser = true;
      description = "Elżbieta Rus";
      extraGroups = [ "wheel" "scanner" "networkmanager" ];
    };
  };

  system.autoUpgrade = {
    enable = true;
    dates = "1:30";
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
