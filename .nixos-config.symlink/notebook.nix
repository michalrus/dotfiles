{ config, pkgs, ... }:

let
  logkeysMapPl = builtins.toFile "logkeys-pl.map" (builtins.readFile ./pkgs/logkeys-pl.map);
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

  powerManagement = {
    cpuFreqGovernor = "performance";
    powerDownCommands = ''
      ${pkgs.procps}/bin/pgrep ssh | while IFS= read -r pid ; do
        [ "$(readlink "/proc/$pid/exe")" = "${pkgs.openssh}/bin/ssh" ] && kill "$pid"
      done
      '';
    powerUpCommands = ''
      ${pkgs.eject}/bin/eject -i on /dev/sr0 # Why isn’t this working‽ How to block the CD eject button?
      '';
  };

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
      imgurbash2 = pkgs.callPackage ./pkgs/imgurbash2.nix {};
      mtr        = pkgs.stdenv.lib.overrideDerivation pkgs.mtr (oldAttrs : {
                     src = pkgs.fetchgit {
                       url = https://github.com/traviscross/mtr.git;
                       rev = "faa1bd87e4325b604223aaa8ad5517872ccb7336";
                       sha256 = "1vvnmzbd5jf9n8wk1g58f7gxfq1sh8ki0w6hj7z9lq9cs59413zb";
                     };
                     buildInputs = [ pkgs.automake ];
                     preConfigure = "./bootstrap.sh";
                   });
      mu         = pkgs.stdenv.lib.overrideDerivation pkgs.mu (oldAttrs : {
                     patches = [ ./pkgs/mu-x-smssync.patch ];
                   });
      st = pkgs.stdenv.lib.overrideDerivation pkgs.st (oldAttrs : {
        patches = [
          (pkgs.fetchpatch { url = "http://st.suckless.org/patches/st-0.6-hidecursor.diff"; sha256 = "1zac3cqi1jkdy6f6g709xdvws1v2k56j2nx95jaakxm795z7k77m"; })
          (pkgs.fetchpatch { url = "http://st.suckless.org/patches/st-0.6-externalpipe.diff"; sha256 = "1q4w4c99qaxz60qjw14fwzc5lzkqkj9wqcdfzvpyz4vx5gxjnhyv"; })
          ./pkgs/st-externalpipe-zombies.patch
          ./pkgs/st-shortcuts.patch
          ./pkgs/st-escape-seqs.patch
          ./pkgs/st-solarized-dark.patch
        ];
        postPatch = ''
          substituteInPlace config.def.h --replace "Liberation Mono:pixelsize=12:antialias=false:autohint=false" "Monospace:pixelsize=15:antialias=true:autohint=true"
          '';
      });
      # take some of the packages from nixpkgs/master definitions
      #youtube-dl = pkgs.callPackage ./pkgs/youtube-dl-master.nix {};
    };
  };

  # List packages installed in system profile. To search by name, run:
  # $ nix-env -qaP | grep wget
  environment.systemPackages = with pkgs; [
    arandr
    aspell
    aspellDicts.en
    aspellDicts.pl
    aspellDicts.de
    awf-gtk
    bc
    calc
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
    imgurbash2
    indent
    isync
    jhead
    jmeter
    jmtpfs
    faad2   # video in Firefox
    ffmpeg
    ffmpegthumbnailer
    gnumake
    gocr
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
    screen
    shared_mime_info
    silver-searcher
    socat
    sox
    st
    stdmanpages
    stdman
    tesseract
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
      extraGroups = [ "wheel" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" ];
    };

    extraGroups.nonet = {};
    extraGroups.wireshark = {};
  };

  security = {
    sudo.extraConfig = ''
      Defaults timestamp_timeout=0
      %users      ALL=(ALL:ALL) NOPASSWD: ${pkgs.systemd}/bin/systemctl start lock-x11-displays
    '';
    setuidPrograms = [ "mtr" ];
    setuidOwners = [
      {
        program = "dumpcap";
        owner = "root";
        group = "wireshark";
        setuid = true;
        setgid = false;
        permissions = "u+rx,g+x";
      }
    ];
  };

}
