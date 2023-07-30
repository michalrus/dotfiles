{ config, flake, pkgs, lib, ... }:

let

  # faster than ‘reset’, see <https://apple.stackexchange.com/a/113168>
  clearAndEraseScrollback = "clear && printf '\\e[3J'";

  turnOffCtrlZSQ = ''
    stty -ixon       # turn off C-s and C-q
    stty susp undef  # turn off C-z
  '';

  binShForEmacs = ''
    # In Emacs TRAMP sessions, use plain ‘/bin/sh’:
    [ "$TERM" != "dumb" ] || exec /bin/sh
  '';

in {

  programs.bash = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    historyControl = [ "ignoredups" "ignorespace" ];
    historyFileSize = 100 * 1000;
    bashrcExtra = ''
      # Run these only for interactive shells (otherwise ‘home-manager-*.service’ silently breaks on NixOS):
      if [[ $- == *i* ]] ; then
        ${binShForEmacs}
        ${turnOffCtrlZSQ}
      fi
    '';
    logoutExtra = clearAndEraseScrollback + "\n";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    history = {
      extended = true;
      ignoreSpace = true;
      ignoreDups = true;
      size = 100 * 1000;
    };
    #syntaxHighlighting.enable = true;  # FIXME: update home-manager
    autocd = false;
    initExtraFirst = ''
      ${binShForEmacs}
      ${turnOffCtrlZSQ}
    '';
    initExtra = ''
      setopt AUTO_PUSHD
      setopt PUSHD_IGNORE_DUPS
      setopt PUSHD_SILENT

      unsetopt EXTENDED_GLOB

      setopt APPEND_HISTORY

      zstyle ':completion:*' menu select

      bindkey "\e[1;5C" forward-word
      bindkey "\e[1;5D" backward-word
      bindkey "\e[1;3C" forward-word
      bindkey "\e[1;3D" backward-word

      bindkey "\e[A" history-beginning-search-backward
      bindkey "\e[B" history-beginning-search-forward
      [[ -n "''${terminfo[kcuu1]}" ]] && bindkey "''${terminfo[kcuu1]}" history-beginning-search-backward
      [[ -n "''${terminfo[kcud1]}" ]] && bindkey "''${terminfo[kcud1]}" history-beginning-search-forward
    '';
    logoutExtra = clearAndEraseScrollback + "\n";
  };

  home.sessionVariablesExtra = ''
    umask 0077
  '';

  programs.bat.enable = true;

  home.shellAliases = rec {
    l = ''ls -lh --color --group-directories-first $([ "$PWD" = "$HOME" ] && echo "" || echo "-A")'';
    ll = l;
    cat = "bat";  # safe to use as cat in pipes
    g = "git";
    d = "dirs -v";
    s = "exec screen -x 7260c3b2-2e3d-4b22-8f8b-ab87de790446";
    clear = clearAndEraseScrollback;
    youtube-dl = "yt-dlp";  # for --argv completion
    spell-password = ''bash -c 'read -s -p "Password: " pw ; echo ; fold -w1 <<<"$pw" | cat -n' '';
  };

  programs.starship = {
    enable = true;
    settings = {
    };
  };

  home.packages = with pkgs; [
    # Has to be managed by home-manager, to trigger installation on Darwin:
    (nerdfonts.override {fonts = ["Iosevka"];})
  ];

  programs.htop.enable = true;

  #programs.direnv.enable = true;
  #programs.direnv.nix-direnv.enable = true;

  home.file.".config/yt-dlp/config".text = ''
    --embed-metadata
    --remux-video mp4
  '';

}
