{ config, pkgs, pkgsUnstable, lib, nix-doom-emacs, ... }:

{
  home.stateVersion = "22.05";

  #programs.direnv.enable = true;
  #programs.direnv.nix-direnv.enable = true;

  programs.htop.enable = true;

  programs.bash.enable = true;

  programs.git = {
    enable = true;
    aliases = rec {
      s = "status";
      d = "diff";
      c = "diff --cached";
      a = "add";
      co = "checkout";
      lg = "log --color --graph --pretty=format:'%Cred%h%Creset %C(bold magenta)%G?%Creset%C(yellow)%d%Creset %s %Cgreen(%ar) %C(bold blue)<%an>%Creset' --abbrev-commit --date-order";
      lga = lg + " --all";
      ff = "merge --ff-only";
    };
    extraConfig = {
      core.safecrlf = false;
      core.commentchar = ";";
      merge.conflictstyle = "diff3";
      fetch.prune = true;
      pull.ff = "only";
      transfer.fsckObjects = true;
    };
  };

  # For whatever reason this doesn’t happen on Darwin:
  home.file.".gitconfig".text = config.xdg.configFile."git/config".text;

  home.file.".gnupg/gpg-agent.conf".text = ''
    default-cache-ttl 0
    max-cache-ttl 0
    default-cache-ttl-ssh 0
    max-cache-ttl-ssh 0
    enable-ssh-support
    no-allow-external-cache
    ignore-cache-for-signing
    pinentry-program ${pkgs.pinentry_mac + "/" + pkgs.pinentry_mac.binaryPath}
  '';

  # FIXME: this still doesn’t work:
  targets.darwin.defaults."org.gpgtools.pinentry-mac".DisableKeychain = true; # what no-allow-external-cache should’ve done

  programs.gpg.enable = true;
  programs.gpg.settings = {
    no-greeting = true;
    keyid-format = "0xlong";
    keyserver = "hkp://keyserver.ubuntu.com";
  };

  programs.zsh = {
    enable = true; # for home.shellAliases
    history.extended = true;
    history.ignoreSpace = true;
    initExtraFirst = ''
      # in Emacs TRAMP sessions, use plain /bin/sh
      [ "$TERM" != "dumb" ] || exec /bin/sh
    '';
    initExtra = ''
      source ${pkgs.zsh-prezto}/share/zsh-prezto/modules/prompt/external/powerlevel10k/config/p10k-classic.zsh

      typeset -g POWERLEVEL9K_LEFT_PROMPT_ELEMENTS=( dir vcs newline prompt_char )
      typeset -g POWERLEVEL9K_RIGHT_PROMPT_ELEMENTS=( status command_execution_time background_jobs direnv context nix_shell time newline )
      typeset -g POWERLEVEL9K_MODE=nerdfont-complete
      typeset -g POWERLEVEL9K_ICON_PADDING=moderate
      typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_PREFIX=
      typeset -g POWERLEVEL9K_MULTILINE_NEWLINE_PROMPT_PREFIX=
      typeset -g POWERLEVEL9K_MULTILINE_LAST_PROMPT_PREFIX=
      typeset -g POWERLEVEL9K_MULTILINE_FIRST_PROMPT_SUFFIX=
      typeset -g POWERLEVEL9K_MULTILINE_NEWLINE_PROMPT_SUFFIX=
      typeset -g POWERLEVEL9K_MULTILINE_LAST_PROMPT_SUFFIX=
      typeset -g POWERLEVEL9K_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL='\uE0B4'
      typeset -g POWERLEVEL9K_RIGHT_PROMPT_FIRST_SEGMENT_START_SYMBOL='\uE0B6'
      typeset -g POWERLEVEL9K_EMPTY_LINE_LEFT_PROMPT_LAST_SEGMENT_END_SYMBOL=
      typeset -g POWERLEVEL9K_VCS_VISUAL_IDENTIFIER_EXPANSION=
      typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_VISUAL_IDENTIFIER_EXPANSION=
      typeset -g POWERLEVEL9K_TIME_VISUAL_IDENTIFIER_EXPANSION=
      typeset -g POWERLEVEL9K_INSTANT_PROMPT=verbose
      typeset -g POWERLEVEL9K_DISABLE_HOT_RELOAD=true
      typeset -g POWERLEVEL9K_DIR_MAX_LENGTH='80%'
      typeset -g POWERLEVEL9K_STATUS_VERBOSE_SIGNAME=true
      typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_THRESHOLD=1
      typeset -g POWERLEVEL9K_COMMAND_EXECUTION_TIME_PRECISION=2

      # Uncomment to add context even locally:
      unset POWERLEVEL9K_CONTEXT_{DEFAULT,SUDO}_{CONTENT,VISUAL_IDENTIFIER}_EXPANSION

      setopt AUTO_PUSHD
      setopt PUSHD_IGNORE_DUPS
      setopt PUSHD_SILENT

      unsetopt EXTENDED_GLOB
      unsetopt AUTO_CD
    '';
  };

  programs.zsh.prezto = {
    enable = true;
    prompt.pwdLength = "long";
    editor.promptContext = true;
    pmodules = [
      "environment" "terminal" "editor"
      "syntax-highlighting" "history-substring-search" "autosuggestions"
      "completion" "prompt"
    ];
    extraConfig = ''
      zstyle ':prezto:module:git:alias' skip 'yes'
      zstyle ':prezto:module:utility' correct 'no'
      zstyle ':prezto:module:prompt' show-return-val 'yes'
    '';
    prompt.theme = "powerlevel10k";
  };

  home.shellAliases = rec {
    l = "ls -lh --color --group-directories-first $([ \"$PWD\" = \"$HOME\" ] && echo \"\" || echo \"-A\")";
    ll = l;
    g = "git";
    d = "dirs -v";
    youtube-dl = "yt-dlp";  # for --argv completion
    spell-password = ''bash -c 'read -s -p "Password: " pw ; echo ; fold -w1 <<<"$pw" | cat -n' '';
  };

  home.packages = with pkgs; [
    # Has to be managed by home-manager, to trigger installation on Darwin:
    (nerdfonts.override { fonts = [ "Iosevka" ]; })

    oath-toolkit
    anki-bin
  ];

  home.file.".config/yt-dlp/config".text = ''
    --embed-metadata
    --remux-video mp4
  '';

  programs.password-store = {
    enable = true;
    package = pkgs.pass;
    settings.PASSWORD_STORE_DIR = "$HOME/.password-store";
  };

  # Browser integration of password-store via Native Messaging:
  programs.browserpass.enable = true;

  programs.doom-emacs = {
    enable = true;
    doomPrivateDir = builtins.path { path = nix-doom-emacs + "/test/doom.d"; };

    emacsPackage = (pkgs.emacs.overrideAttrs (drv: {
      postInstall = (drv.postInstall or "") + (let
        doWrap = exe: let unwrapped = dirOf exe + "/." + baseNameOf exe + "-no-env"; in ''
          mv $out/${exe} $out/${unwrapped}
          cat >$out/${exe} <<EOF
          #!/bin/sh
          # Wrap execution to happen inside a login Bash shell, to set all our custom Nix env.:
          exec ${pkgs.stdenv.shell} -l -c 'exec $out/${unwrapped} "\$@"' -- "\$@"
          EOF
          chmod +x $out/${exe}
        '';
      in ''
        ${doWrap "bin/emacs-28.1"}
        ${doWrap "Applications/Emacs.app/Contents/MacOS/Emacs"}
      '');
    }));
  };

}
