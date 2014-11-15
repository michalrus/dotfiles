unalias -a

. ~/.mshell-common

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

shopt -s checkwinsize

[ -r /etc/bash_completion ] && . /etc/bash_completion

[ -r /usr/share/doc/pkgfile/command-not-found.bash ] && . /usr/share/doc/pkgfile/command-not-found.bash

. ~/.local/share/git-prompt.sh

my_git_ps() {
	local tmp="$(__git_ps1 '%s')"
	if [ -n "${tmp}" ] ; then
		local d="\e[1;${1}m"
		local l="\e[0;${2}m"
		echo -e "${d}:(${l}${tmp}${d})"
	fi
}

if [ "$EUID" -eq 0 ] ; then
	PS1='\[\033[01;35m\]\u\[\033[01;31m\]@\[\033[01;35m\]'$MSHELL_HOSTNAME'\[\033[31;01m\]:\[\033[01;35m\]\w$(my_git_ps 31 35)\[\033[31;01m\]\$\[\033[00m\] '
else
	PS1='\[\033[01;36m\]\u\[\033[01;34m\]@\[\033[01;36m\]'$MSHELL_HOSTNAME'\[\033[34;01m\]:\[\033[01;36m\]\w$(my_git_ps 34 36)\[\033[34;01m\]\$\[\033[00m\] '
fi

export CLICOLOR='1'

command -v dircolors >/dev/null && eval "$(dircolors ~/.dircolors)"
