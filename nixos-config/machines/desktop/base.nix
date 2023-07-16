{ inputs, config, lib, pkgs, ... }:

{
  imports = [
    ../../modules
    ../common-x86.nix
    ./modules/openvpn-michalrus_com.nix
    ./modules/cups-reenable.nix
  ];

  powerManagement.cpuFreqGovernor = lib.mkOverride 999 "performance"; # basically lib.mkDefault, but this one is already used in power-management.nix…

  networking = {
    hostName = lib.mkDefault "nixos";
    extraHosts = "127.0.0.1 ${config.networking.hostName}";
    firewall.nonetGroup.enable = lib.mkDefault true;

    networkmanager.dns = "none";
    nameservers = [
      "1.1.1.1"
      "1.0.0.1"
    ];
  };

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
    printing = {
      enable = true;
      drivers = [ pkgs.gutenprint pkgs.hplip pkgs.epson-escpr ];
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
    alsaUtils
    audacity
    awf
    calibre
    cdparanoia
    cdrkit
    cool-retro-term
    devede
    diff-pdf
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
    pdfarranger
    mpv
    ntfs3g
    octave
    pavucontrol
    pcmanfm
    pdfgrep
    pinentry-gtk2
    python3Packages.livestreamer
    rtmpdump
    samba
    scantailor
    shellcheck
    shntool
    simple-scan
    timidity
    utox
    winetricks
    x264
    xarchiver
    xsane
    inputs.self.packages.${pkgs.system}.yt-dlp
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
    dotfiles-old.profiles = [ "base" ];
    openssh.authorizedKeys.keyFiles = [ ../../../dotfiles/michalrus/base/.ssh/authorized_keys.d/michalrus_notebook.pub ];
  };
}
