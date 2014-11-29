unalias -m '*'

. ~/.mshell-common

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

[ -r /usr/share/doc/pkgfile/command-not-found.zsh ] && . /usr/share/doc/pkgfile/command-not-found.zsh

autoload -U compinit && compinit
zstyle ':completion:*' menu select
zstyle ':completion:*:hosts' hosts ''
setopt completealiases

setopt HIST_IGNORE_DUPS
setopt APPEND_HISTORY
setopt EXTENDED_HISTORY
export HISTFILE=~/.zsh_history
export HISTSIZE=100000
export SAVEHIST=$HISTSIZE

. ~/.local/share/git-prompt.sh

precmd() {
  local last_exitcode="$?"

  if [ "$EUID" -ne 0 ] ; then
    local L='%{%F{cyan}%B%}'
    local M='%{%b%F{cyan}%}'
    local D='%{%F{blue}%B%}'
  else
    local L='%{%F{magenta}%B%}'
    local M='%{%b%F{magenta}%}'
    local D='%{%F{red}%B%}'
  fi

  local error='%{%f%K{red}%}'
  local reset='%{%f%k%b%}'

  PS1=''

  PS1+="$D($M"
  [ "$last_exitcode" -ne 0 ] && PS1+="$error"
  PS1+="$last_exitcode$reset$D)"

  PS1+="$L%n$D@$L$__mshell_hostname$D:$L%~"

  local git="$(__git_ps1 '%s')"
  if [ -n "$git" ] ; then
    PS1+="$D:($M$git$D)"
  fi

  PS1+="$D%# $reset"

  PS2="$M%_$D> $reset"
}

autoload -U up-line-or-beginning-search
autoload -U down-line-or-beginning-search
zle -N up-line-or-beginning-search
zle -N down-line-or-beginning-search

bindkey "\e[A" up-line-or-beginning-search
bindkey "\e[B" down-line-or-beginning-search

bindkey "\e[1~" beginning-of-line
bindkey "\e[3~" delete-char
bindkey "\e[4~" end-of-line
bindkey "\e[5~" beginning-of-buffer-or-history
bindkey "\e[6~" end-of-buffer-or-history

ignore-widget() {
}
zle -N ignore-widget
bindkey "\e[24~" ignore-widget    # F12
