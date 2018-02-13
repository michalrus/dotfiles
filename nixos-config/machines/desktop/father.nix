{ config, lib, pkgs, ... }:

{
  imports = [
    ./base-parents.nix
    ./modules/xfce.nix
  ];

  virtualisation.virtualbox.host.enable = true;

  services.tor = {
    enable = true;
    client.enable = true;
    torifiedUsers = [ "robert-tor" ];
  };

  environment.systemPackages = with pkgs; [
    nixos-unstable.ltris
    pcmanfm
    albatross
    gdrivefs
    truecrypt
    mimms
    bomi
    streamlink
    aspell
    aspellDicts.pl
    pdfmod
    perlPackages.PDFAPI2
    fbida
    di
    duc
    libarchive
    firefox
    hunspell
    tor
    qalculate-gtk
    afterstep
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
