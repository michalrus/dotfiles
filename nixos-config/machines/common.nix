{ config, lib, pkgs, ... }:

{
  nix = {
    extraOptions = ''
      gc-keep-outputs = true
      auto-optimise-store = true
    '';

    useSandbox = true;

    gc = {
      automatic = lib.mkForce false;
      dates = "daily";
      options = "--delete-older-than 30d"; # Is this enough?
    };
  };

  networking.firewall.rejectPackets = true;

  programs = {
    mtr.enable = true;
    zsh = {
      enable = true;
      enableCompletion = true;
    };
    bash.enableCompletion = true;
  };

  services = {
    haveged.enable = true;

    smartd.enable = lib.mkDefault true;

    # Because of this insanity… → https://github.com/NixOS/nixpkgs/pull/16021
    logind.extraConfig = ''
      KillUserProcesses=yes
      UserStopDelaySec=0
    '';

    journald.extraConfig = ''
      SystemMaxUse=200M
    '';
  };

  users = {
    mutableUsers = lib.mkDefault false;
    defaultUserShell = "/run/current-system/sw/bin/zsh";
  };

  environment.variables.PATH = [ "$HOME/.bin" ];

  environment.systemPackages = with pkgs; [
    (hiPrio netcat-openbsd)
    (hiPrio arping)
    arpoison
    bc
    bindfs
    calc
    conntrack-tools
    daemonize
    ddrescue
    dos2unix
    easyrsa
    entr
    evemu
    evtest
    ext4magic
    file
    gcc
    git
    gnumake
    hdparm
    htop
    httperf
    hwinfo
    inetutils
    inotify-tools
    iotop
    iw
    jq
    libfaketime
    lm_sensors
    lshw
    lsof
    mkpasswd
    moreutils
    ncdu
    nmap
    openssl
    p7zip
    pciutils
    pv
    smartmontools
    socat
    sqlite-interactive
    strace
    stress-ng
    tcpdump
    unzip
    usbutils
    wget
    which
    wirelesstools
    zip
  ];

  security = {
    pam.services.su.requireWheel = true;

    # TODO: re-enable after <https://github.com/containers/libpod/pull/5550>
    #       <https://github.com/containers/crun/commit/a171c3bb7316c29b0e2d207327e0e39b8243b538>
    #hideProcessInformation = true;

    sudo.extraConfig = ''
      Defaults timestamp_timeout=0
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch -k
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild switch -k --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot -k
      %wheel ALL=(root) NOPASSWD: ${config.system.build.nixos-rebuild}/bin/nixos-rebuild boot -k --upgrade
      %wheel ALL=(root) NOPASSWD: ${config.nix.package.out}/bin/nix-collect-garbage -d
    '';

    wrappers.fping = {
      source = "${pkgs.fping}/bin/fping";
      capabilities = "cap_net_raw+p";
      owner = "root";
      group = "root";
    };

    wrappers.traceroute = {
      source = "${pkgs.traceroute}/bin/traceroute";
      capabilities = "cap_net_raw+p";
      owner = "root";
      group = "root";
    };
  };

  # Stability!
  system.autoUpgrade.enable = lib.mkDefault false;
}
