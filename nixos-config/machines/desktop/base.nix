{ config, lib, pkgs, ... }:

{
  imports = [
    ../../modules
    ../common-x86.nix
    ./modules/openvpn.nix
    ./modules/cups-reenable.nix
  ];

  powerManagement.cpuFreqGovernor = lib.mkOverride 999 "performance"; # basically lib.mkDefault, but this one is already used in power-management.nix…

  networking = {
    hostName = lib.mkDefault "nixos";
    extraHosts = "127.0.0.1 ${config.networking.hostName}";
    firewall.nonetGroup.enable = lib.mkDefault true;
  };

  environment.etc."resolv.conf.head".text = ''
    nameserver 8.8.8.8
    nameserver 8.8.4.4
  '';

  hardware = {
    sane.enable = true;
    opengl.driSupport32Bit = true; # for Wine
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

  # https://lwn.net/Articles/572911/
  boot.kernel.sysctl."vm.dirty_background_bytes" = 16 * 1024 * 1024;
  boot.kernel.sysctl."vm.dirty_bytes" = 16 * 1024 * 1024;

  services = {
    printing =
      {
        enable = true;
      } // (if lib.nixpkgsVersion >= "17.09" then {
        drivers = [ pkgs.gutenprint ];
      } else {
        gutenprint = true;
      });

    logkeys' = {
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
      synaptics = {
        enable = lib.mkDefault true;
        twoFingerScroll = true;
        tapButtons = true;
        fingersMap = [1 3 2];
      };
    };
  };

  # Use GTK 2 in LibreOffice. 3 has some menu rendering problems.
  environment.variables."SAL_USE_VCLPLUGIN" = "gtk";

  environment.systemPackages = with pkgs; [
    (nixos-unstable.youtube-dl)
    (wine.override { pulseaudioSupport = true; })
    alsaUtils
    arandr
    audacity
    awf
    calibre
    cdparanoia
    cdrkit
    cool-retro-term
    devede
    dvdauthor
    evince
    flac
    gimp
    gnome3.aisleriot
    gnome3.baobab
    gnome3.cheese
    gpac
    gparted
    gtk2  # Why? Icon cache! See #20874.
    handbrake
    inkscape
    lame
    libjpeg
    libnotify
    libreoffice
    mpv
    ntfs3g
    octave
    pavucontrol
    pcmanfm
    pdfgrep
    pdfshuffler
    pinentry
    pinentry
    python34Packages.livestreamer
    rfkill
    rtmpdump
    samba
    scantailor
    shellcheck
    shntool
    simple-scan
    timidity
    transmission_gtk
    unclutter
    utox
    winetricks
    wmctrl
    x264
    xarchiver
    xautolock
    xbanish
    xcape
    xclip
    xdo
    xdotool
    xorg.xbacklight
    xorg.xdpyinfo
    xorg.xev
    xorg.xhost
    xorg.xmodmap
    xpad
    xrandr-invert-colors
    xsane
    xsel
    xtitle
  ];

  fonts = {
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      terminus_font
      unifont
      unifont_upper
    ];
  };

  users.users.root = {
    dotfiles.profiles = [ "base" ];
    openssh.authorizedKeys.keyFiles = [ ../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
  };
}
