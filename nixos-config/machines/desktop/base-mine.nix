{ config, lib, pkgs, ... }:

{
  imports = [
    ./base.nix
    ./modules/mpd.nix
    ./modules/android.nix
    ./modules/emacs.nix
    ./modules/malicious-hosts.nix
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

  environment.systemPackages = with pkgs; [
    (haskellPackages.ghcWithHoogle (hs: []))
    chromium
    compton
    cool-retro-term
    dunst
    gettext
    ghostscript
    breeze-qt4 breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    gnome3.zenity
    haskellPackages.hlint
    michalrus.gettext-emacs
    michalrus.intero.nix-shim
    michalrus.leksah
    michalrus.lemonbar-xft
    networkmanagerapplet
    nixos-unstable.alacritty
    nixos-unstable.octave
    rofi
    shellcheck
    i3status
    stalonetray
    sxhkd
    termite
  ];

  programs = {
    ssh.startAgent = false;
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
      HandlePowerKey=suspend
    '';

    lockX11Displays.enable = true;

    screen.usersAlways = [];

    tor = {
      enable = true;
      client.enable = true;
      torifiedUsers = [ "mfin" ];
    };

    xserver = {
      xkbOptions = "caps:hyper,numpad:microsoft";

      synaptics = {
        maxSpeed = "4.0";
        accelFactor = "0.02";
        additionalOptions = ''
          Option "VertScrollDelta" "-114"
          Option "HorizScrollDelta" "-114"
        '';
      };

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
      windowManager.i3.package = pkgs.michalrus.i3;

      displayManager.xserverArgs = [ "-ardelay" "150" "-arinterval" "8" ];
    };
  };

  fonts.fonts = with pkgs; [
    anonymousPro
    hack-font
    iosevka
    font-awesome-ttf
  ];

  users = let

    immutableDotfiles =  map (p: "${../../../dotfiles}/${p}");
    mutableDotfiles = u: map (p: "${u.home}/.dotfiles/dotfiles/${p}");

  in {
    guestAccount = {
      enable = true;
      skeleton = "/home/guest.skel";
      groups = [ "audio" "nonet" "scanner" "networkmanager" "vboxusers" ];
    };

    users.guest.dotfiles = immutableDotfiles [ "base" "i3" "michalrus/guest" ];

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles = mutableDotfiles config.users.users.m [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages = with pkgs; [
        aegisub
        gnome3.dconf   # so that GnuCash prefs can be changed
        gnucash26
        isync
        openjdk8   # for nofatty
        lilypond
        michalrus.transcribe
        nixos-unstable.bitcoin
        nixos-unstable.electrum
        stack
        (texlive.combine {
          inherit (texlive) scheme-small latexmk titlesec tocloft todonotes cleveref lipsum biblatex logreq cm-super csquotes pgfplots adjustbox collectbox ccicons polski placeins xstring pdfpages unicode-math filehook textpos marvosym progressbar lm-math ucharcat;
          #gregorio = pkgs.michalrus.gregorio.forTexlive;
        })
      ];
    };

    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      uid = 1337;
      description = "Michal Rus (work)";
      extraGroups = [ "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles = immutableDotfiles [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/work/di" "i3" "emacs" ];
      packages = with pkgs; [
        jetbrains.idea-community
        michalrus.hubstaff
        nixos-unstable.discord
        nixos-unstable.minecraft
        nixos-unstable.skype
        openjdk8   # for nofatty
        pgadmin
      ];
    };

    extraUsers.mfin = {
      hashedPassword = "$6$YYkKeZ8M56$EZh1jGZbAnPaVKwnQebJ40ojqNShuU3Np2rWiPOOrfA1vXlFPCGbmbVZwOfRyCMU5y83ggkA1/jIG7Zzg6iD10";
      isNormalUser = true;
      uid = 1347;
      description = "Michal Rus (fin)";
      extraGroups = [ "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles = immutableDotfiles [ "base" "i3" "emacs" ];
    };
  };

  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };
}
