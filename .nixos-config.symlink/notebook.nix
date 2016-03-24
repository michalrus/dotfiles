{ config, pkgs, ... }:

let
  logkeysMapPl = builtins.toFile "logkeys-pl.map" (builtins.readFile ./pkgs/logkeys-master/pl.map);
in
{
  nix = {
    useChroot = true; # https://nixos.org/nixos/manual/options.html#opt-nix.useChroot

    trustedBinaryCaches = [
      http://hydra.nixos.org
    ];

    binaryCachePublicKeys = [
      "hydra.nixos.org-1:CNHJZBh9K4tP3EKF6FkkgeVYsS3ohTl+oS0Qa8bezVs="
    ];
  };

  imports =
    [
      ./hardware-configuration.nix
      # import all files matching ./local/*.nix
    ] ++ builtins.map (n: ./local + ("/" + n)) (builtins.filter (n: builtins.substring ((builtins.stringLength n) - 4) 4 n == ".nix") (builtins.attrNames (builtins.readDir ./local)));

  boot = {
    loader.grub = {
      enable = true;
      version = 2;
      device = "/dev/sda";
    };
    initrd.luks.devices = [ { name = "crypt"; device = "/dev/sda2"; } ];
    tmpOnTmpfs = true;
  };

  powerManagement.cpuFreqGovernor = "performance";

  networking = rec {
    hostName = "nixos";
    extraHosts = "127.0.0.1 ${hostName}";
    nameservers = [ "8.8.8.8" "8.8.4.4" ];
    firewall = {
      enable = true;
      allowPing = true;
      allowedTCPPorts = [ ];
      extraCommands = ''
        iptables  -A OUTPUT -m owner --gid-owner nonet -j REJECT --reject-with icmp-port-unreachable
        ip6tables -A OUTPUT -m owner --gid-owner nonet -j REJECT --reject-with icmp6-port-unreachable
        '';
    };
    wireless.enable = true;
  };

  i18n = {
    consoleFont = "Lat2-Terminus16";
    consoleKeyMap = "us";
    defaultLocale = "en_US.UTF-8";
  };

  time.timeZone = "Europe/Warsaw";

  hardware = {
    sane.enable = true;
    opengl.driSupport32Bit = true; # for Wine
  };

  nixpkgs.config = {
    allowBroken = true;
    packageOverrides = pkgs: {
      awf-gtk    = pkgs.callPackage ./pkgs/awf-gtk.nix {};
      mu         = pkgs.stdenv.lib.overrideDerivation pkgs.mu (oldAttrs : {
                     patches = [ ./pkgs/mu/x-smssync.patch ];
                   });
      # take some of the packages from nixpkgs/master definitions
      #logkeys    = pkgs.callPackage ./pkgs/logkeys-master/default.nix {};
      #youtube-dl = pkgs.callPackage ./pkgs/youtube-dl-master.nix {};
    };
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    arandr
    awf-gtk
    bc
    calibre
    cloc
    compton
    cool-retro-term
    dmenu
    dos2unix
    dunst
    emacs
    evince
    exiv2
    file
    gcc
    ghc
    git
    gitstats
    gnome.gnome_icon_theme
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.adwaita-icon-theme
    gnome3.gnome_themes_standard
    gnucash26
    gnupg1compat
    gnupg
    gpac
    graphicsmagick
    htop
    hwinfo
    i3lock
    i3status
    imagemagick
    imgurbash
    indent
    isync
    jhead
    jmeter
    jmtpfs
    faad2   # video in Firefox
    ffmpeg
    ffmpegthumbnailer
    gnumake
    libjpeg
    libmtp
    libnotify
    logkeys
    lshw
    lsof
    man_db
    manpages
    maven
    mkpasswd
    moreutils
    mpv
    mtr
    mu
    netcat-openbsd
    nix-prefetch-scripts
    nix-repl
    nmap
    normalize
    oathToolkit
    openjdk8
    p7zip
    pass
    pciutils
    perlPackages.ConfigTiny
    perlPackages.DateCalc
    perlPackages.JSON
    perlPackages.LWP
    perlPackages.LWPProtocolHttps
    pinentry
    pkgs.firefoxWrapper
    poppler_utils
    posix_man_pages
    python34Packages.livestreamer
    rxvt_unicode-with-plugins
    screen
    shared_mime_info
    socat
    sox
    stdmanpages
    stdman
    transmission_gtk
    unzip
    urlwatch
    usbutils
    utox
    wget
    which
    whois
    wine
    winetricks
    wireshark
    wmctrl
    wrk
    x264
    xautolock
    xcape
    xclip
    xdotool
    xorg.xbacklight
    xorg.xev
    xorg.xdpyinfo
    xorg.xmodmap
    xrandr-invert-colors
    xsel
    youtube-dl
    zip
  ];

  environment.shellInit = ''
    export GTK2_RC_FILES=${pkgs.gnome3.gnome_themes_standard}/share/themes/Adwaita/gtk-2.0/gtkrc
    '';

  programs = {
    zsh.enable = true;
    bash.enableCompletion = true;
    ssh.startAgent = false;
  };

  services = {
    logind.extraConfig = ''
        HandleLidSwitch=suspend
        HandlePowerKey=suspend
      '';

    virtualboxHost.enable = true;

    printing.enable = false;

    locate.enable = true;

    xserver = {
      enable = true;
      layout = "pl";
      xkbOptions = "ctrl:nocaps,compose:caps";
      synaptics = {
        enable = true;
        maxSpeed = "4.0";
        accelFactor = "0.02";
        twoFingerScroll = true;
        tapButtons = true;
        fingersMap = [1 3 2];
        additionalOptions = ''
          Option "VertScrollDelta" "-114"
          Option "HorizScrollDelta" "-114"
          '';
      };

      displayManager.lightdm.enable = true;
      desktopManager.xterm.enable = false;
      windowManager.i3.enable = true;
      startGnuPGAgent = true;

      displayManager.xserverArgs = [ "-ardelay" "150" "-arinterval" "8" ];
    };
  };

  systemd.services."lock-x11-displays" = {
    description = "Lock all X11 displays using i3lock (not showing notifications)";
    # This can’t be done in powerManagement.powerDownCommands, because
    # its systemd unit is of the “oneshot” type; we need “forking”.
    # Besides, this service can be used in WM config — DRY!
    serviceConfig.Type = "forking";
    wantedBy = [ "sleep.target" ];
    before = [ "sleep.target" ];
    path = with pkgs; [ procps bash i3lock ];
    script = ''
      # I need ${pkgs.sudo}. But this still doesn’t guarantee that
      # /var/setuid-wrappers/sudo will exist… :-)
      pgrep -f xsession | while read p ; do
        printf '%s %s\n' \
          $(cat /proc/$p/environ | tr '\0' '\n' | grep ^DISPLAY | cut -d = -f 2) \
          $(cat /proc/$p/environ | tr '\0' '\n' | grep ^USER    | cut -d = -f 2)
      done | sort | uniq | while read DISPLAY USER ; do
        [ -z "$(grep "^$USER:" /etc/shadow | cut -d : -f 2)" ] && continue # if password empty
        export DISPLAY
        /var/setuid-wrappers/sudo --background -u $USER bash -c \
          'pkill -u $USER -USR1 dunst
           i3lock -n -c 000000 || true
           pkill -u $USER -USR2 dunst'
      done
      '';
  };

  systemd.services."logkeys" = {
    description = "Log all keys pressed on all keyboards";
    serviceConfig.Type = "forking";
    wantedBy = [ "multi-user.target" ];
    path = with pkgs; [ logkeys ];
    script = ''
      ls /dev/input/by-path | grep kbd | while IFS= read -r inp ; do
        rinp="$(readlink -f "/dev/input/by-path/$inp")"
        logkeys --start --device="$rinp" --output=/var/log/logkeys.log --keymap="${logkeysMapPl}"
        # why is the following not configurable?!
        rm /var/run/logkeys.pid
      done
      '';
  };
  # reload logkeys when a new USB keyboard is connected
  services.udev.extraRules = ''
    ACTION=="add", SUBSYSTEM=="input", SUBSYSTEMS=="usb", ATTRS{authorized}=="1", RUN+="${pkgs.systemd}/bin/systemctl restart logkeys.service"
    '';

  fonts = {
    enableFontDir = true;
    enableGhostscriptFonts = true;
    fonts = with pkgs; [
      anonymousPro
      corefonts
      hack-font
      inconsolata
      terminus_font
      unifont
      unifont_upper
    ];
  };

  users = {
    mutableUsers = false;
    defaultUserShell = "/run/current-system/sw/bin/zsh";

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "nonet" "scanner" "networkmanager" "vboxusers" ];
    };

    extraGroups.nonet = {};
  };

  security = {
    sudo.extraConfig = ''
      Defaults timestamp_timeout=0
      %users      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl start lock-x11-displays
    '';
    setuidPrograms = [ "mtr" ];
  };

}
