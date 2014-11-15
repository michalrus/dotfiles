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

MSHELL_SYS='?'
MSHELL_GNU=false

if ( uname | grep -i linux >/dev/null ); then
	MSHELL_SYS='linux'
	MSHELL_GNU=true
elif ( uname | grep -i darwin >/dev/null ); then
	MSHELL_SYS='macosx'
	( which gls >/dev/null ) && MSHELL_GNU=true
elif ( uname | grep -i bsd >/dev/null ) ; then
	MSHELL_SYS='bsd'
elif ( uname | grep -i mingw >/dev/null ) ; then
	MSHELL_SYS='windows'
elif ( uname | grep -i cygwin >/dev/null ); then
	MSHELL_SYS='cygwin'
	MSHELL_GNU=true
fi

MSHELL_LS='ls'

if ( $MSHELL_GNU ) ; then
	eval "$(dircolors "${HOME}/.dircolors")"
	[ $MSHELL_SYS = 'macosx' ] && MSHELL_LS='gls'
elif [ $MSHELL_SYS = 'macosx' ] ; then
	export CLICOLOR='1'
fi

d () {
	local HIDDEN=true
	[ "$PWD" = "$HOME" ] && [ $# -eq 0 ] && local HIDDEN=false

	if ( $MSHELL_GNU ) ; then
		$MSHELL_LS --color --group-directories-first -lh $($HIDDEN && echo "-A") "$@"
	elif [ $MSHELL_SYS = 'macosx' ] ; then
		$MSHELL_LS -lhG $($HIDDEN && echo "-A") "$@"
	elif [ $MSHELL_SYS = 'windows' ] ; then
		$MSHELL_LS --color -lh $($HIDDEN && echo "-A") "$@"
	elif [ $MSHELL_SYS = 'bsd' ] ; then
		$MSHELL_LS -lhG $($HIDDEN && echo "-A") "$@"
	fi
}

dl () {
	d "$@" | less -S -R -I
}
