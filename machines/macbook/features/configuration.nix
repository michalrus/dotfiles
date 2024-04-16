{
  pkgs,
  flake,
  lib,
  config,
  ...
}: {
  #
  # sudo launchctl config user umask 077
  #

  nix.configureBuildUsers = true;
  nix.nrBuildUsers = 32;

  # To test the linux-builder, try building this (not in cache.nixos.org):
  #   linux-builder-test = callPackage ({hello}: hello.overrideAttrs (old: { pname = "hello-modified"; })) {};
  nix.settings.extra-trusted-users = ["unused--avoid-silly-linux-builder-assertion"];
  nix.linux-builder = {
    enable = true;
  };

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
    #gitstatus  # for powerlevel10k Zsh prompt (or else, it will download&install its own binary blobs in ~/.cache)
    bat
    ripgrep

    mpv
    flake.packages.${pkgs.system}.yt-dlp

    flake.packages.${pkgs.system}.noise

    # FIXME: for some reason it's no longer visible in Launchpad – and the custom path can be set via defaults as well
    #
    # Substitute IINA’s built-in `youtube-dl` with the freshest one:
    #(iina.overrideAttrs (drv: {
    #  installPhase =
    #    (drv.installPhase or "")
    #    + ''
    #      rm $out/Applications/IINA.app/Contents/MacOS/youtube-dl
    #      ln -s ${flake.packages.${pkgs.system}.yt-dlp}/bin/yt-dlp $out/Applications/IINA.app/Contents/MacOS/youtube-dl
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
