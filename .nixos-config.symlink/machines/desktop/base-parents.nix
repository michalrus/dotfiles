{ config, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./modules/android.nix
  ];

  networking.networkmanager.enable = true;

  time.timeZone = "Europe/Warsaw";
  i18n.defaultLocale = "pl_PL.UTF-8";

  hardware = {
    sane.extraConfig.pixma = "bjnp://10.0.1.5";
  };

  nixpkgs.config = {
    allowUnfree = true; # M$ fonts, Skype™ and similar nonsense.

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
    xserver = {
      synaptics = {
        maxSpeed = "5.0";
        accelFactor = "0.025";
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

  fonts.fonts = with pkgs; [
    corefonts
    vistafonts
  ];

  users.extraUsers.elzbieta = {
    hashedPassword = "$6$W/KppVZSY$.vf1jfCd6H0tOJwRwmUwJeMSkmg/MyDUlNpx3IRHWjmLpyXyg5quW0VRBX4QwGp00MIT6Nw2nODs.JhleHblz1";
    isNormalUser = true;
    description = "Elżbieta Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" ];
  };

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
