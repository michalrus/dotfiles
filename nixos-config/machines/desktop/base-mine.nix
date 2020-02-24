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
    ./my-hosts.nix
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

  networking.networkmanager.enable = true;

  # For true Firefox smooth scrolling with touchpad.
  environment.variables.MOZ_USE_XINPUT2 = "1";

  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithHoogle (hs: []))
    aegisub
    alacritty
    autocutsel
    blueman
    breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    compton
    cool-retro-term
    dhall
    dunst
    feh
    gettext
    ghostscript
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.zenity
    haskellPackages.hlint
    i3status
    michalrus.gettext-emacs
    michalrus.gnucash26
    networkmanagerapplet
    octave
    pdfpc
    peek
    rofi
    sqlint
    stalonetray
    termite
    watchexec
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

    lockX11Displays.enable = true;

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

    xserver = {
      xkbOptions = "caps:hyper,numpad:microsoft";

      synaptics.enable = lib.mkForce false;

      libinput = {
        enable = true;
        accelSpeed = "0.1";
        naturalScrolling = true;
      };

      config = ''
        Section "InputClass"
          Identifier "libinput pointer catchall"
          MatchIsPointer "on"
          MatchDevicePath "/dev/input/event*"
          Option "NaturalScrolling" "on"
          Driver "libinput"
        EndSection
      '';

      autoRepeatDelay = 150;
      autoRepeatInterval = 8;

      displayManager.lightdm = {
        enable = true;
        background = "${../../../dotfiles/michalrus/trash/.wallpapers/rainbow.png}";
        greeters.gtk = {
          theme.package = pkgs.breeze-gtk;
          theme.name = "Breeze";
          # No way to choose hicolor as a fallback. ⇒ https://github.com/NixOS/nixpkgs/issues/30694
          #iconTheme.package = pkgs.breeze-icons;
          #iconTheme.name = "breeze";
        };
      };
      desktopManager.xterm.enable = false;
      windowManager.i3.enable = true;
      windowManager.i3.package = pkgs.i3;
    };

    sqlite-dump = [{
      source = "/home/m/.shared/nofatty/data.db";
      destination = "/home/m/Archive/Personal/Backup/nofatty.sql";
      runAt = "*:0/15"; # every 15 mins
      user = "m";
      group = "users";
    }];
  };

  fonts = {
    fontconfig.ultimate.enable = true;

    fonts = with pkgs; [
      anonymousPro
      hack-font
      iosevka-bin
      font-awesome-ttf
    ];
  };

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "vboxusers" ];
    };

    users.guest = {
      dotfiles.profiles = [ "base" "i3" "michalrus/guest" ];
      packages = with pkgs; [
        unfree.google-chrome
        unfree.zoom-us
      ];
    };

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles.base = "${config.users.users.m.home}/.dotfiles/dotfiles";
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages' = with pkgs; [
        chromium
        electrum
        michalrus.monero-gui-tor
        isync
        lilypond
        (wrapFirefox (michalrus.hardened-firefox-unwrapped.override {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.m;
          extraPrefs = michalrus.hardened-firefox-unwrapped.cfgEnableDRM;
        }) {})
        openjdk8   # for nofatty
        #unfree.michalrus.transcribe
        (texlive.combine {
          inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages unicode-math filehook textpos marvosym progressbar lm-math ucharcat
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
      description = "Michal Rus (w)";
      extraGroups = [ "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/work/di" "i3" "emacs" ];
      packages' = with pkgs; [
        (wrapFirefox (michalrus.hardened-firefox-unwrapped.override {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.mw;
          extraPrefs = michalrus.hardened-firefox-unwrapped.cfgEnableDRM;
        }) {})
        chromium
        openjdk8   # for nofatty
        pgadmin
        unfree.michalrus.hubstaff
        #unfree.michalrus.transcribe
      ];
    };

    extraUsers.md = {
      hashedPassword = "$6$YYkKeZ8M56$EZh1jGZbAnPaVKwnQebJ40ojqNShuU3Np2rWiPOOrfA1vXlFPCGbmbVZwOfRyCMU5y83ggkA1/jIG7Zzg6iD10";
      isNormalUser = true;
      uid = 1347;
      description = "Michal Rus (d)";
      extraGroups = [ "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "michalrus/tor" "i3" "emacs" ];
      packages' = with pkgs; [
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

  fileSystems."/var/home/mw/.shared-sync" = {
    device = "/var/home/m/Archive/Personal/Shared-mw";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };
}
