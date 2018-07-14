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
    alacritty
    autocutsel
    breeze-qt5 breeze-icons pkgs.hicolor_icon_theme kde-gtk-config breeze-gtk
    chromium
    compton
    cool-retro-term
    dhall
    dunst
    feh
    gettext
    ghostscript
    gnome3.zenity
    haskellPackages.hlint
    i3status
    michalrus.gettext-emacs
    michalrus.sqlint
    networkmanagerapplet
    octave
    pdfpc
    peek
    rofi
    stalonetray
    termite
    watchexec
  ];

  programs = {
    wireshark.enable = true;
    wireshark.package = pkgs.wireshark-gtk;
    ssh.startAgent = false;
  };

  virtualisation.virtualbox.host.enable = true;

  hardware.android.automount = let user = config.users.users.m; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Phone";
  };

  hardware.enableSomagicEasyCAP = true;

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

    users.guest.dotfiles.profiles = [ "base" "i3" "michalrus/guest" ];

    extraUsers.m = {
      hashedPassword = "$6$wO42jkhqerm$kl.qIl5USrzqAZOIkXdicrBLBgVwka2Dz81nc.aNsNJZREXY.02XxPdL1FiTCcuVP2K/DSmXqAQ3aPbri/v.g1";
      isNormalUser = true;
      uid = 31337;
      description = "Michal Rus";
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles.base = "${config.users.users.m.home}/.dotfiles/dotfiles";
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/personal" "i3" "emacs" ];
      packages' = with pkgs; [
        aegisub
        electrum
        gnome3.dconf   # so that GnuCash prefs can be changed
        isync
        lilypond
        michalrus.gnucash26
        openjdk8   # for nofatty
        unfree.michalrus.transcribe
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
      dotfiles.profiles = [ "base" "michalrus/base" "michalrus/desktop" "git-annex" "michalrus/work/di" "i3" "emacs" ];
      packages' = with pkgs; [
        jetbrains.idea-community
        openjdk8   # for nofatty
        pgadmin
        unfree.michalrus.discord
        unfree.michalrus.hubstaff
        unfree.nixos-unstable.minecraft
        unfree.nixos-unstable.skype
        unfree.nixos-unstable.vscode
      ];
    };

    extraUsers.mfin = {
      hashedPassword = "$6$YYkKeZ8M56$EZh1jGZbAnPaVKwnQebJ40ojqNShuU3Np2rWiPOOrfA1vXlFPCGbmbVZwOfRyCMU5y83ggkA1/jIG7Zzg6iD10";
      isNormalUser = true;
      uid = 1347;
      description = "Michal Rus (fin)";
      extraGroups = [ "nonet" "scanner" "networkmanager" "vboxusers" "wireshark" "cdrom" ];
      dotfiles.profiles = [ "base" "michalrus/base" "i3" "emacs" ];
    };
  };

  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };
}
