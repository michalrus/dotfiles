{
  pkgs,
  pkgsUnstable,
  lib,
  config,
  ...
}: {
  # Nix configuration ------------------------------------------------------------------------------

  imports = [
    ./programs-mtr.nix
  ];

  #
  # sudo launchctl config user umask 077
  #

  nix.configureBuildUsers = true;
  nix.nrBuildUsers = 32;
  nix.settings.auto-optimise-store = true;

  # Additional IOG (Cardano) binary cache:
  nix.settings.substituters = lib.mkForce ["https://cache.nixos.org" "https://cache.iog.io" ];
  nix.settings.trusted-public-keys = lib.mkForce [
    "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
    "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
  ];

  nix.settings.trusted-users = lib.mkForce ["root"];

  nix.nixPath = lib.mkForce [
    {darwin-config = "${config.environment.darwinConfig}";}
    {nixpkgs = pkgs.path;} #  "/nix/var/nix/profiles/per-user/root/channels"
  ];

  nix.package = pkgsUnstable.nixUnstable; # go back to pkgsStable once they have >2.10

  nix.extraOptions =
    ''
      experimental-features = nix-command flakes
      keep-outputs = true
      keep-derivations = true
    ''
    + lib.optionalString (pkgs.system == "aarch64-darwin") ''
      extra-platforms = x86_64-darwin aarch64-darwin
    '';

  networking.hostName = "macbook";
  # On the local network:
  networking.localHostName = "Michals-MacBook-Pro";
  networking.computerName = "Michal’s MacBook Pro";

  # Create /etc/bashrc that loads the nix-darwin environment.
  programs.bash.enable = true;
  programs.bash.enableCompletion = true;

  programs.zsh.enable = true;
  programs.zsh.enableCompletion = true;
  programs.zsh.enableBashCompletion = true;

  # Auto upgrade nix package and the daemon service.
  services.nix-daemon.enable = true;

  programs.nix-index.enable = true;

  environment.darwinConfig = config.users.users.m.home + "/.dotfiles/macos/flake.nix";

  environment.systemPackages = with pkgs; [
    alejandra
    coreutils
    findutils
    gnugrep
    gnused
    gnutar
    gnumake
    curl
    wget
    gnupg
    ffmpeg-full
    imagemagickBig
    exiftool
    jq
    calc
    nano
    git
    #pkgsUnstable.gitstatus  # for powerlevel10k Zsh prompt (or else, it will download&install its own binary blobs in ~/.cache)
    bat

    mpv
    (pkgsUnstable.yt-dlp.override {withAlias = true;})

    # FIXME: for some reason it's no longer visible in Launchpad – and the custom path can be set via defaults as well
    #
    # Substitute IINA’s built-in `youtube-dl` with the freshest from `pkgsUnstable`:
    #(iina.overrideAttrs (drv: {
    #  installPhase =
    #    (drv.installPhase or "")
    #    + ''
    #      rm $out/Applications/IINA.app/Contents/MacOS/youtube-dl
    #      ln -s ${pkgsUnstable.yt-dlp}/bin/yt-dlp $out/Applications/IINA.app/Contents/MacOS/youtube-dl
    #    '';
    #}))

    python3
  ];

  programs.gnupg.agent.enable = true;
  programs.gnupg.agent.enableSSHSupport = true;

  environment.variables = {
  };

  system.activationScripts.postActivation.text = ''
    echo >&2 "system defaults (custom)..."
    defaults write /Library/Preferences/com.apple.loginwindow SHOWOTHERUSERS_MANAGED -bool NO
  '';

  ## Keyboard
  #system.keyboard.enableKeyMapping = true;
  #system.keyboard.remapCapsLockToEscape = true;

  # Add ability to used TouchID for sudo authentication
  #security.pam.enableSudoTouchIdAuth = true;
}
