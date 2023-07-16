{ inputs, config, lib, pkgs, ... }:

let
  pkgs-23_05 = inputs.nixpkgs.legacyPackages.${pkgs.system};
  unfree-23_05 = import inputs.nixpkgs { inherit (pkgs) system; config.allowUnfree = true; };
in

{
  imports = [
    ./base.nix
    ./modules/android.nix
    ./modules/emacs.nix
    ./modules/malicious-hosts.nix
    ./modules/hardened-chromium.nix
    ./modules/hardened-firefox.nix
    ./modules/firefox-autocomplete.nix
    ./modules/openvpn-nordvpn.nix
    ./modules/transmission.nix
    ./modules/hibernate-on-low-battery.nix
    ./modules/proaudio.nix
    ./modules/udev-remap-keyboard.nix
    ./modules/libvirt.nix

    # TODO: rethink
    ./modules/lock-vts.nix

    ./modules/window-managers.nix
  ];

  nix = {
    package = pkgs.nixos-unstable.nixUnstable; # 2.11.1
    extraOptions = ''
      experimental-features = nix-command flakes
    '';

    binaryCaches = [
      "https://cache.iog.io"
    ];

    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
    ];
  };

  boot.tmpOnTmpfs = true;

  # For building RPi3 system on an x86 laptop:
  boot.binfmt.emulatedSystems = [ "aarch64-linux" ];

  boot.kernel.sysctl = {
    "fs.inotify.max_user_watches" = "1048576";
  };

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

    2049 # NFS v4 server
  ];

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
    inputs.self.packages.${pkgs.system}.gettext-emacs
    inputs.self.packages.${pkgs.system}.noise
    networkmanagerapplet
    octave
    pandoc
    pdfpc
    (python3.withPackages (p: with p; [ scipy geopy python-lsp-server requests pylint matplotlib tkinter beautifulsoup4 aiohttp humanize protobuf ]))
    python3Packages.livereload
    qjoypad
    nixos-unstable.retroarchFull
    rpcs3
    rustup
    rust-analyzer
    sqlint
    termite
    tigervnc
    inputs.self.packages.${pkgs.system}.transcribe
    tunctl
    virt-manager # Gtk3 for QEMU/KVM
    vscodium
    watchexec
    speedread
    xdg_utils
    yubikey-manager
    yubikey-manager-qt
    yubikey-personalization
    yubikey-personalization-gui
    yubioath-desktop
  ];

  programs = {
    wireshark.enable = true;
    wireshark.package = pkgs.wireshark-qt;
    ssh.startAgent = false;
  };

  services.udev.packages = [

    # YubiKey
    pkgs.yubikey-personalization

    # Ledger Nano S Plus
    pkgs.ledger-udev-rules
    (pkgs.writeTextDir "lib/udev/rules.d/19-ledger-generic.rules" ''
      # The newest Ledger rule (not yet in Nixpkgs):
      SUBSYSTEMS=="usb", ATTRS{idVendor}=="2c97", ATTRS{idProduct}=="0005|5000|5001|5002|5003|5004|5005|5006|5007|5008|5009|500a|500b|500c|500d|500e|500f|5010|5011|5012|5013|5014|5015|5016|5017|5018|5019|501a|501b|501c|501d|501e|501f", TAG+="uaccess", TAG+="udev-acl"
    '')

  ];

  virtualisation.podman.enable = true;

  programs.dconf.enable = true; # for virt-manager
  # <https://github.com/kholia/OSX-KVM/blob/master/kvm.conf>
  boot.extraModprobeConfig = ''
    options kvm_intel nested=1
    options kvm_intel emulate_invalid_guest_state=0
    options kvm ignore_msrs=1 report_ignored_msrs=0
  '';

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

    # <https://nixos.wiki/wiki/Bluetooth#System-Wide_PulseAudio>:
    extraConfig = ''
      load-module module-bluetooth-policy
      load-module module-bluetooth-discover
      ## module fails to load with
      ##   module-bluez5-device.c: Failed to get device path from module arguments
      ##   module.c: Failed to load module "module-bluez5-device" (argument: ""): initialization failed.
      # load-module module-bluez5-device
      # load-module module-bluez5-discover
    '';
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

    nfs.server.enable = true;
    nfs.server.exports = ''
      /home/mw/VMs/Shared     127.0.0.1/8(insecure,rw,sync,no_subtree_check,all_squash,anonuid=1337,anongid=100)
    '';

    logind.lidSwitch = "suspend";
    logind.extraConfig = ''
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

    blueman.enable = true;

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
      extraGroups = [ "wheel" "audio" "nonet" "scanner" "networkmanager" "libvirtd" "wireshark" "cdrom" "video" ];
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
          gregorio = inputs.self.packages.${pkgs.system}.gregorio.forTexlive;
        })
      ];
    };

    extraUsers.mw = {
      hashedPassword = "$6$EDtlcw2d9XVBOw$Y0SLSpFnAc/tc3z8/Y4cQK/p.Vuqkwz0HHBkYcDAlUI3lHOFJQBj0cscE30qs2YoxsoUwOxIno0g4zhZUsZ7R1";
      isNormalUser = true;
      uid = 1337;

      description = "Michal Rus (w)";
      extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "libvirtd" "wireshark" "cdrom" "video" ];
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

    extraUsers.md = {
      hashedPassword = "$6$YYkKeZ8M56$EZh1jGZbAnPaVKwnQebJ40ojqNShuU3Np2rWiPOOrfA1vXlFPCGbmbVZwOfRyCMU5y83ggkA1/jIG7Zzg6iD10";
      isNormalUser = true;
      uid = 1347;
      description = "Michal Rus (d)";
      extraGroups = [ "audio" "nonet" "scanner" "networkmanager" "wireshark" "cdrom" "video" ];
      dotfiles-old.profiles = [ "base" "michalrus/base" "michalrus/desktop" "michalrus/tor" "i3" "emacs" ];
      packages = with pkgs; [
        electrum
        (hardened-firefox.makeWrapped {
          localAutocompletePort = config.services.firefox-autocomplete.userPorts.md;
          extraPrefs = ''
            // Override those for more privacy:
            lockPref("privacy.resistFingerprinting", true);
            lockPref("dom.enable_performance", false);
            lockPref("network.cookie.lifetimePolicy", 2); // The cookie expires at the end of the session.
          '';
        })
      ];
    };
  };

  fileSystems."/var/home/mw/.shared" = {
    device = "/var/home/m/.shared";
    fsType = "fuse.bindfs";
    options = [ "map=m/mw" ];
  };

  fileSystems."/var/home/mw/VM-Shared/win10" = {
    device = "//192.168.122.239/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=unused" "password=" "vers=2.0"
      "uid=mw" "forceuid" "gid=users" "forcegid"
      "file_mode=0644" "dir_mode=0755"
      "nofail" "_netdev" "x-systemd.automount"
      "x-systemd.mount-timeout=5s" "x-systemd.device-timeout=5s" "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/ubuntu" = {
    device = "//192.168.122.114/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=unused" "password=" "vers=2.0"
      "uid=mw" "forceuid" "gid=users" "forcegid"
      "file_mode=0644" "dir_mode=0755"
      "nofail" "_netdev" "x-systemd.automount"
      "x-systemd.mount-timeout=5s" "x-systemd.device-timeout=5s" "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/macos11" = {
    device = "//192.168.122.75/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=mw" "password=dupa.8" "vers=2.0"
      "uid=mw" "forceuid" "gid=users" "forcegid"
      "file_mode=0644" "dir_mode=0755"
      "nofail" "_netdev" "x-systemd.automount"
      "x-systemd.mount-timeout=5s" "x-systemd.device-timeout=5s" "x-systemd.idle-timeout=2min"
    ];
  };

  fileSystems."/var/home/mw/VM-Shared/macos11-dev" = {
    device = "//192.168.122.77/Shared";
    fsType = "cifs";
    options = [
      "rw"
      "username=mw" "password=dupa.8" "vers=2.0"
      "uid=mw" "forceuid" "gid=users" "forcegid"
      "file_mode=0644" "dir_mode=0755"
      "nofail" "_netdev" "x-systemd.automount"
      "x-systemd.mount-timeout=5s" "x-systemd.device-timeout=5s" "x-systemd.idle-timeout=2min"
    ];
  };
}
