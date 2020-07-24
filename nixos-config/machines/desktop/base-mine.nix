{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./modules/mpd.nix
    ./modules/android.nix
    ./modules/emacs.nix
    ./modules/malicious-hosts.nix
    ./modules/hardened-chromium.nix
    ./modules/firefox-autocomplete.nix
    ./modules/openvpn-expressvpn.nix
    ./modules/transmission.nix
    ./modules/hibernate-on-low-battery.nix
    ./modules/proaudio.nix
    ./my-hosts.nix

    # TODO: use `virtualisation.podman.enable = true;` on >20.03
    ../../modules/podman.nix

    ./modules/no-display-manager/i3.nix
    ./modules/no-display-manager/sway.nix
    ./modules/no-display-manager/lock-vts.nix
    ./modules/no-display-manager/udev-remap-keyboard.nix
  ];

  boot.tmpOnTmpfs = true;

  i18n = {
    defaultLocale = "en_US.UTF-8";
    extraLocaleSettings = {
      LC_TIME  = "en_DK.UTF-8"; # for 24 h
      LC_PAPER = "en_DK.UTF-8"; # for ISO A4 instead of Letter
    };
  };

  powerManagement = {
    powerDownCommands = ''
      ${pkgs.procps}/bin/pgrep ssh | while IFS= read -r pid ; do
        if [ "$(readlink "/proc/$pid/exe")" = "${pkgs.openssh}/bin/ssh" ] ; then
          kill "$pid"
        fi
      done
    '';
    powerUpCommands = ''
      ${pkgs.eject}/bin/eject -i on /dev/sr0 # Why isn’t this working‽ How to block the CD eject button?
    '';
  };

  networking.networkmanager = {
    enable = true;
    dhcp = "dhclient"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
  };

  networking.firewall.allowedTCPPorts = [
    12345 # python -m SimpleHTTPServer 12345
  ];

  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithHoogle (hs: []))
    (nixos-unstable.wine.override { pulseaudioSupport = true; })
    aegisub
    alacritty
    blueman
    breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    brightnessctl
    cool-retro-term
    dhall
    dvdbackup
    feh
    gdb
    binutils
    gettext
    ghostscript
    gist
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.zenity
    haskellPackages.hlint
    k3b
    libxml2
    michalrus.gettext-emacs
    michalrus.gnucash26
    michalrus.noise
    networkmanagerapplet
    octave
    pandoc
    pdfpc
    (python3.withPackages (p: with p; [ scipy matplotlib tkinter ]))
    python3Packages.livereload
    rustup
    sqlint
    termite
    watchexec
    speedread
    xdg_utils
  ];

  programs = {
    wireshark.enable = true;
    wireshark.package = pkgs.wireshark-qt;
    ssh.startAgent = false;
  };

  virtualisation.virtualbox.host.enable = true;

  hardware.android.automount = let user = config.users.users.m; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Phone";
  };

  hardware.enableSomagicEasyCAP = true;

  hardware.bluetooth.enable = true;

  hardware.pulseaudio = {
    enable = true;
    systemWide = true; # Running multiple concurrent user sessions on different TTYs, I want their audio mixed.
    support32Bit = true;
    package = pkgs.pulseaudioFull;
  };

  systemd.extraConfig = ''
    DefaultCPUAccounting=yes
    DefaultBlockIOAccounting=yes
    DefaultMemoryAccounting=yes
    DefaultTasksAccounting=yes
    DefaultIPAccounting=yes
  '';

  services = {
    logind.extraConfig = ''
      HandleLidSwitch=suspend
      HandlePowerKey=suspend
    '';

    screen.usersAlways = [];

    tor = {
      enable = true;
      client.enable = true;
      torifiedUsers = [
        { username = "md"; allowedLocalPorts = [ config.services.firefox-autocomplete.userPorts.md ]; }
      ];
    };

    firefox-autocomplete.userPorts = {
      m = 9114;
      mw = 9115;
      md = 9116;
    };

    # No global X11! See <./modules/no-display-manager/i3.nix>
    xserver.enable = lib.mkForce false;

    sqlite-dump = [{
      source = "/home/m/.shared/nofatty/data.db";
      destination = "/home/m/Archive/Personal/Backup/nofatty.sql";
      runAt = "*:0/15"; # every 15 mins
      user = "m";
      group = "users";
    }];
  };

  fonts.fonts = with pkgs; [
    anonymousPro
    hack-font
    iosevka-bin
    font-awesome-ttf
    font-awesome
  ];

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "vboxusers" "video" ];
    };

    users.guest = {
      dotfiles.profiles = [ "base" "i3" "michalrus/guest" ];
      packages = with pkgs; [
        unfree.google-chrome
        unfree.skypeforlinux
        unfree.michalrus.zoom-us # recording patch
      ];
    };

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;

      # TODO: (for Podman) use `virtualisation.containers.users = [ "m" ];` on >20.03
      subUidRanges = [{ startUid = 100000; count = 65536; }];
      subGidRanges = [{ startGid = 100000; count = 65536; }];

      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" "video" ];
      dotfiles.base = "${config.users.users.m.home}/.dotfiles/dotfiles";
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages = with pkgs; [
        chromium
        electrum
        isync
        jetbrains.idea-community
        lilypond
        monero-gui
        (wrapFirefox (michalrus.hardened-firefox-unwrapped.override {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.m;
          extraPrefs = michalrus.hardened-firefox-unwrapped.cfgEnableDRM;
        }) {})
        openjdk8   # for nofatty
        (texlive.combine {
          inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages unicode-math filehook textpos marvosym fontawesome progressbar lm-math ucharcat
            # for Org-mode export to PDF
            wrapfig wasysym
            ;
          #gregorio = pkgs.michalrus.gregorio.forTexlive;
        })
      ];
    };

    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      uid = 1337;

      # TODO: (for Podman) use `virtualisation.containers.users = [ "m" ];` on >20.03
      subUidRanges = [{ startUid = 200000; count = 65536; }];
      subGidRanges = [{ startGid = 200000; count = 65536; }];

      description = "Michal Rus (w)";
      extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" "video" ];
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/work/ig" "i3" "emacs" ];
      packages = with pkgs; [
        (wrapFirefox (michalrus.hardened-firefox-unwrapped.override {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.mw;
          extraPrefs = michalrus.hardened-firefox-unwrapped.cfgEnableDRM;
        }) {})
        chromium
        jetbrains.idea-community
        ansible
        openjdk8
        protobuf
        sbt
        qgis
        josm
        unfree.skypeforlinux
        unfree.michalrus.zoom-us # recording patch
      ];
    };

    extraUsers.md = {
      hashedPassword = "$6$YYkKeZ8M56$EZh1jGZbAnPaVKwnQebJ40ojqNShuU3Np2rWiPOOrfA1vXlFPCGbmbVZwOfRyCMU5y83ggkA1/jIG7Zzg6iD10";
      isNormalUser = true;
      uid = 1347;
      description = "Michal Rus (d)";
      extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" "video" ];
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "michalrus/tor" "i3" "emacs" ];
      packages = with pkgs; [
        (wrapFirefox (michalrus.hardened-firefox-unwrapped.override {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.md;
          extraPrefs = ''
            // Override those for more privacy:
            lockPref("privacy.resistFingerprinting", true);
            lockPref("dom.enable_performance", false);
            lockPref("network.cookie.lifetimePolicy", 2); // The cookie expires at the end of the session.
          '';
        }) {})
        michalrus.wasabi-wallet
      ];
    };
  };

  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };
}
