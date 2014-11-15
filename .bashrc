unalias -a

. ~/.mshell-common

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s checkwinsize

[ -r /etc/bash_completion ] && . /etc/bash_completion

[ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash

. ~/.local/share/git-prompt.sh

__mshell_set_prompt() {
  if [ "$EUID" -ne 0 ] ; then
    local L='\[\e[01;36m\]'
    local M='\[\e[00;36m\]'
    local D='\[\e[01;34m\]'
  else
    local L='\[\e[01;35m\]'
    local M='\[\e[00;35m\]'
    local D='\[\e[01;31m\]'
  fi

  local reset='\[\e[00m\]'

  PS1="$L\\u$D@$L$__mshell_hostname$D:$L\\w"

  local git="$(__git_ps1 '%s')"
  if [ -n "$git" ] ; then
    PS1+="$D:($M$git$D)"
  fi

  PS1+="$D\\$ $reset"
}

PROMPT_COMMAND='__mshell_set_prompt'
