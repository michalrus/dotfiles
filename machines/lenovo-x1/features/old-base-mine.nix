{ flake, config, lib, pkgs, ... }:

let
  pkgs-23_05 = flake.inputs.nixpkgs.legacyPackages.${pkgs.system};
  unfree-23_05 = import flake.inputs.nixpkgs { inherit (pkgs) system; config.allowUnfree = true; };
in

{
  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = "1048576";
  };

  networking.networkmanager = {
    enable = true;
    dhcp = "dhclient"; # <https://forum.salixos.org/viewtopic.php?f=30&t=7284>
  };

  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithHoogle (hs: []))
    (wine.override { pulseaudioSupport = true; })
    acpitool
    aegisub
    alacritty
    anki
    bat
    binutils
    breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    brightnessctl
    cool-retro-term
    dhall
    dvdbackup
    feh
    gdb
    ipcalc
    gettext
    ghostscript
    gist
    gnome3.adwaita-icon-theme # for resizable cursors
    gnome3.dconf   # so that GnuCash prefs can be changed
    gnome3.zenity
    haskellPackages.hlint
    httrack
    k3b
    libguestfs
    libxml2
    flake.packages.${pkgs.system}.gettext-emacs
    flake.packages.${pkgs.system}.noise
    networkmanagerapplet
    octave
    pandoc
    pdfpc
    (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
    python3Packages.livereload
    qjoypad
    pkgs-23_05.retroarchFull
    rpcs3
    rustup
    rust-analyzer
    sqlint
    termite
    tigervnc
    flake.packages.${pkgs.system}.transcribe
    tunctl
    vscodium
    watchexec
    speedread
    xdg_utils
  ];

  programs = {
    wireshark.enable = true;
    wireshark.package = pkgs.wireshark-qt;
    ssh.startAgent = false;
  };

  hardware.pulseaudio = {
    enable = true;
    systemWide = true; # Running multiple concurrent user sessions on different TTYs, I want their audio mixed.
    support32Bit = true;
  };

  # For system-wide PulseAudio: <https://github.com/NixOS/nixpkgs/issues/114399>
  system.activationScripts.fix-pulse-permissions = ''
    chmod 755 /run/pulse
  '';

  systemd.extraConfig = ''
    DefaultCPUAccounting=yes
    DefaultBlockIOAccounting=yes
    DefaultMemoryAccounting=yes
    DefaultTasksAccounting=yes
    DefaultIPAccounting=yes
  '';

  services = {
    logind.lidSwitch = "suspend";
    logind.extraConfig = ''
      HandlePowerKey=suspend
    '';

    # No global X11! See <./modules/no-display-manager/i3.nix>
    xserver.enable = lib.mkForce false;
  };

  fonts.fonts = with pkgs; [
    anonymousPro
    hack-font
    iosevka-bin
    font-awesome-ttf
    font-awesome
    google-fonts
  ];

  users = {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "video" ];
    };

    users.guest = {
      dotfiles-old.profiles = [ "base" "i3" "michalrus/guest" ];
      packages = with pkgs; [
        unfree-23_05.google-chrome
        unfree-23_05.skypeforlinux
        unfree-23_05.zoom-us
        unfree-23_05.unrar
      ];
    };

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;

      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
      dotfiles-old.base = "${config.users.users.m.home}/.dotfiles/dotfiles";
      dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages = with pkgs; [
        pkgs-23_05.chromium
        electrum
        pkgs-23_05.gnucash
        isync
        jetbrains.idea-community
        unfree-23_05.skypeforlinux
        lilypond
        monero-gui
        (hardened-firefox.makeWrapped {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.m;
          extraPrefs = hardened-firefox.unwrapped.cfgEnableDRM;
        })
        openjdk8   # for nofatty
        (texlive.combine {
          inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages unicode-math filehook textpos marvosym fontawesome progressbar lm-math ucharcat pdfjam
            # for Org-mode export to PDF
            wrapfig wasysym
            ;
          gregorio = flake.packages.${pkgs.system}.gregorio.forTexlive;
        })
      ];
    };

    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      uid = 1337;

      description = "Michal Rus (w)";
      extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
      dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/work/iohk" "i3" "emacs" ];
      packages = with pkgs; [
        (hardened-firefox.makeWrapped {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.mw;
          extraPrefs = hardened-firefox.unwrapped.cfgEnableDRM;
        })
        pkgs-23_05.chromium
        #jetbrains.idea-community
        unfree-23_05.jetbrains.webstorm
        yarn
        nodejs
        #ansible_2_8
        openjdk8
        protobuf
        sbt
        qgis
        josm
        unfree-23_05.skypeforlinux
        unfree-23_05.zoom-us
      ];
    };
  };

  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };
}
