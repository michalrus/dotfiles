{ config, lib, pkgs, ... }:

{
  imports = [
    ./base-parents.nix
    ./modules/xfce.nix
  ];

  virtualisation.virtualbox.host = {
    enable = true;
    enableExtensionPack = true;
  };

  system.autoUpgrade = {
    enable = true;
    dates = "15:16";
  };

  services.tor = {
    enable = true;
    client.enable = true;
    torifiedUsers = [ { username = "robert-tor"; allowedLocalPorts = []; } ];
  };

  environment.systemPackages = with pkgs; [
    pcmanfm
    albatross
    gdrivefs
    mimms
    streamlink
    aspell
    aspellDicts.pl
    pdfmod
    perlPackages.PDFAPI2
    fbida
    di
    libarchive
    firefox
    hunspell
    tor
    qalculate-gtk
    afterstep
    softether
    winetricks
    shotwell
    rhythmbox
    gnome3.zenity
    gnunet
    filezilla
  ];

  users.extraUsers.robert = {
    hashedPassword = "$6$rcYySsCDE$X/ilZ3Z4/3dUQ0pPXwnStOQQAsGuoCNY26/29oA4vY6gj.9ZpFYnpaiCUXl4w4sEBdtzqze42LePiIFx51cmM1";
    isNormalUser = true;
    description = "Robert Rus";
    extraGroups = [ "wheel" "scanner" "networkmanager" "vboxusers" "cdrom" ];
    dotfiles.profiles = [ "base" "xfce" "git-annex" "robertrus" ];
  };

  users.extraUsers.robert-tor = {
    hashedPassword = "$6$rcYySsCDE$X/ilZ3Z4/3dUQ0pPXwnStOQQAsGuoCNY26/29oA4vY6gj.9ZpFYnpaiCUXl4w4sEBdtzqze42LePiIFx51cmM1";
    isNormalUser = true;
    description = "Robert Rus (TOR)";
    extraGroups = [ "scanner" "networkmanager" "cdrom" ];
    dotfiles.profiles = [ "base" "xfce" "robertrus" ];
  };

  hardware.android.automount = let user = config.users.users.robert; in {
    enable = true;
    user = user.name;
    point = "${user.home}/Telefon";
  };
}
