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
    initExtra = ''
      ${binShForEmacs}
      ${turnOffCtrlZSQ}

      # Needed for Startship’s ‘cmd_duration’:
      source ${pkgs.fetchFromGitHub {
        owner = "rcaloras"; repo = "bash-preexec";
        rev = "fb23d474b330fac4704b9baa7cfa98745ab1015b";  # Mar 13, 2023
        hash = "sha256-sLHTmplKIcsCNqG+JoJKc58sWimqqfJwUxEteRxFf4w=";
      }}/bash-preexec.sh
    '';
    logoutExtra = clearAndEraseScrollback + "\n";
  };

  programs.zsh = {
    enable = true;
    enableCompletion = true;
    enableVteIntegration = true;
    enableSyntaxHighlighting = true;
    history = {
      extended = true;
      ignoreSpace = true;
      ignoreDups = true;
      size = 100 * 1000;
    };
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

      # Auto-quote URLs when pasting them:
      autoload -U url-quote-magic bracketed-paste-url-magic
      zle -N self-insert url-quote-magic
      zle -N bracketed-paste bracketed-paste-url-magic
    '';
    logoutExtra = clearAndEraseScrollback + "\n";
  };

  home.sessionVariablesExtra = ''
    umask 0077
  '';

  programs.dircolors.enable = true;

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

  programs.starship.enable = true;

  home.file.".config/starship.toml".text = ''
    add_newline = true

    format = """
    $status\
    [\\(](bold bright-blue)\
    $time\
    ([·](bright-blue)$cmd_duration)\
    [\\)](bold bright-blue)\
    $username\
    [@](bold bright-blue)\
    $hostname\
    [:](bold bright-blue)\
    $directory\
    ([\\(](bold bright-blue)\
    $git_branch$git_commit$git_state$git_metrics$git_status\
    [\\)](bold bright-blue))
    $nix_shell$character\
    """

    [status]
    disabled = false
    format = '[\(](bold bright-blue)[$int(·$signal_name)]($style)[\)](bold bright-blue)'
    style = 'bg:red fg:bright-white'

    [cmd_duration]
    min_time = 0
    show_milliseconds = false
    format = '[$duration]($style)'
    show_notifications = true
    min_time_to_notify = 5_000
    style = 'cyan bold'

    [time]
    disabled = false
    format = '[$time]($style)'
    style = 'cyan'

    [username]
    show_always = true
    format = '[$user]($style)'
    style_user = 'cyan'
    style_root = 'bg:purple bright-white bold blink'

    [hostname]
    ssh_only = false
    trim_at = ""
    format = '[$hostname]($style)'
    style = 'cyan'

    [directory]
    format = '[$path]($style)'
    truncate_to_repo = false
    truncation_symbol = '…/'
    truncation_length = 3
    fish_style_pwd_dir_length = 1

    [git_branch]
    always_show_remote = true
    format = '[$branch(:$remote_name)]($style)'
    truncation_length = 25
    style = 'cyan'

    # [git_commit]

    [git_commit]
    format = '[$hash$tag]($style)'
    style = 'cyan'
    commit_hash_length = 11

    [git_state]
    format = ' \([$state( $progress_current/$progress_total)]($style)\)'

    [git_status]
    format = '( [$all_status$ahead_behind]($style))'
    up_to_date = ""

    [nix_shell]
    symbol = '❄ '
    format = '$symbol'

    [character]
    success_symbol = '[❯](bold bright-blue)'
    error_symbol = '[❯](bold red)'
  '';

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
