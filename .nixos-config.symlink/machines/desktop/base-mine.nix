{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./modules/media.nix
    ./modules/android.nix
    ./modules/emacs.nix
    ./modules/malicious-hosts.nix
    ./my-wifi-passwords.nix
  ];

  boot.tmpOnTmpfs = true;

  powerManagement = {
    powerDownCommands = ''
      ${pkgs.procps}/bin/pgrep ssh | while IFS= read -r pid ; do
        [ "$(readlink "/proc/$pid/exe")" = "${pkgs.openssh}/bin/ssh" ] && kill "$pid"
      done
    '';
    powerUpCommands = ''
      ${pkgs.eject}/bin/eject -i on /dev/sr0 # Why isn’t this working‽ How to block the CD eject button?
    '';
  };

  networking = {
    connman = {
      enable = true;
      # https://wiki.archlinux.org/index.php/Connman#Avoid_changing_the_hostname
      extraConfig = ''
        [General]
        AllowHostnameUpdates=false
      '';
    };
  };

  environment.systemPackages = with pkgs; [
    arandr
    awf
    cdrkit
    chromium
    compton
    conkeror
    cool-retro-term
    dmenu
    dunst
    evince
    firefox
    gettext
    gettext-emacs
    ghostscript
    gnome.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnucash26
    gparted
    (haskellPackages.ghcWithPackages (haskellPackages: with haskellPackages; [
      stack
    ]))
    i3status
    isync
    libnotify
    mu
    oathToolkit
    octave
    openjdk8
    pass
    pavucontrol
    pinentry
    scala
    st
    (texlive.combine {
      inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages unicode-math filehook textpos marvosym progressbar lm-math;
      gregorio = pkgs.gregorio.forTexlive;
    })
    transmission_gtk
    utox
    visualvm
    (wine.override { pulseaudioSupport = true; })
    winetricks
    wmctrl
    xarchiver
    xautolock
    xcape
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

  environment.variables."GTK2_RC_FILES" =
    "${pkgs.gnome3.gnome_themes_standard}/share/themes/Adwaita/gtk-2.0/gtkrc";

  programs = {
    ssh.startAgent = false;
    wireshark.enable = true;
  };

  virtualisation.virtualbox.host.enable = true;

  hardware.android.automount = let user = config.users.users.m; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Phone";
  };

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandlePowerKey=hibernate
    '';

    lockX11Displays.enable = true;

    xserver = {
      xkbOptions = "ctrl:nocaps,compose:caps";

      synaptics = {
        maxSpeed = "4.0";
        accelFactor = "0.02";
        additionalOptions = ''
          Option "VertScrollDelta" "-114"
          Option "HorizScrollDelta" "-114"
        '';
      };

      displayManager.lightdm.enable = true;
      desktopManager.xterm.enable = false;
      windowManager.i3.enable = true;

      displayManager.xserverArgs = [ "-ardelay" "150" "-arinterval" "8" ];
    };

    locate = {
      enable = true;
      includeStore = true;
      interval = "0/4:15"; # every 4 hours at 15th minute
    };
  };

  fonts.fonts = with pkgs; [
    anonymousPro
    hack-font
    inconsolata
  ];

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "vboxusers" ];
    };

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      linger = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" ];
    };

    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      linger = true;
      uid = 1337;
      description = "Michal Rus (work)";
      extraGroups = [ "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" ];
    };
  };

  system.autoUpgrade.enable = false;

  # The NixOS release to be compatible with for stateful data such as databases.
  system.stateVersion = "16.09";
}
