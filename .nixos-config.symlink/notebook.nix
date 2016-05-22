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
    pulseaudio = {
      enable = true;
      support32Bit = true; # for Wine
    };
  };

  nixpkgs.config = {
    allowBroken = true;
    pulseaudio = true;
    packageOverrides = super: let self = super.pkgs; in {
      awf-gtk            = (import ./pkgs/awf-gtk.nix super self);
      imgurbash2         = (import ./pkgs/imgurbash2.nix super self);
      conkeror-unwrapped = (import ./pkgs/conkeror-unwrapped.nix super self);
      conky              = (import ./pkgs/conky super self);
      mtr                = (import ./pkgs/mtr.nix super self);
      mu                 = (import ./pkgs/mu super self);
      st                 = (import ./pkgs/st super self);
      visualvm           = (import ./pkgs/visualvm.nix super self);
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
    conkeror-unwrapped
    conky
    cool-retro-term
    dmenu
    dos2unix
    dunst
    emacs
    evince
    exiv2
    file
    gcc
    gimp
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
    (haskellPackages.ghcWithHoogle (haskellPackages: with haskellPackages; [
      cabal-install ghc-mod happy hasktags hindent hlint parallel stylish-haskell turtle
    ]))
    htop
    hwinfo
    i3lock
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
    nix-zsh-completions
    nmap
    normalize
    oathToolkit
    openjdk8
    p7zip
    pass
    pavucontrol
    pciutils
    perlPackages.ConfigTiny
    perlPackages.DateCalc
    perlPackages.JSON
    perlPackages.LWP
    perlPackages.LWPProtocolHttps
    pinentry
    #pkgs.firefoxWrapper
    ((wrapFirefox.override { libpulseaudio = libpulseaudio.out; }) firefox-unwrapped { }) # temporary solution for https://github.com/NixOS/nixpkgs/issues/15126
    poppler_utils
    posix_man_pages
    python34Packages.livestreamer
    rtmpdump
    scala
    screen
    shared_mime_info
    silver-searcher
    socat
    sox
    st
    stdmanpages
    stdman
    tesseract
    (texlive.combine {
      inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons;
    })
    transmission_gtk
    unzip
    urlwatch
    usbutils
    utox
    visualvm
    wget
    which
    whois
    (wine.override { pulseaudioSupport = true; })
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

    haveged.enable = true;

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
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.nix.package.out}/bin/nix-collect-garbage -d
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
