. ~/.mshell-common

if [ -e /etc/bash_completion ] ; then
	. /etc/bash_completion
fi

unalias -a

HAVE_GIT_PS='no'

if ( __git_ps1 >/dev/null 2>&1 ); then
	HAVE_GIT_PS='yes'
	export GIT_PS1_SHOWDIRTYSTATE=1
	export GIT_PS1_SHOWSTASHSTATE=1
	export GIT_PS1_SHOWUNTRACKEDFILES=1
	export GIT_PS1_SHOWUPSTREAM='verbose'
fi

my_git_ps() {
	if [ "$HAVE_GIT_PS" == 'yes' ] ; then
		local tmp="$(__git_ps1 '%s')"
		if [ -n "${tmp}" ] ; then
			local d="\e[1;${1}m"
			local l="\e[0;${2}m"
			echo -e "${d}:(${l}${tmp}${d})"
		fi
	fi
}

if [ "$EUID" -eq 0 ] ; then
	PS1='\[\033[01;35m\]\u\[\033[01;31m\]@\[\033[01;35m\]\h\[\033[31;01m\]:\[\033[01;35m\]\w$(my_git_ps 31 35)\[\033[31;01m\]\$\[\033[00m\] '
else
	PS1='\[\033[01;36m\]\u\[\033[01;34m\]@\[\033[01;36m\]\h\[\033[34;01m\]:\[\033[01;36m\]\w$(my_git_ps 34 36)\[\033[34;01m\]\$\[\033[00m\] '
fi

alias mshell='curl -L -o "${HOME}/.mshell.tgz" "https://michalrus.com/mshell" && tar -xzvf "${HOME}/.mshell.tgz" --no-same-owner -C "${HOME}/"'

alias s='exec screen -d -r'
alias nano='nano -UwT 4'
alias nchmod='chmod -R u=rwX,g=rX,o=rX'
alias pchmod='chmod -R u=rwX,g=,o='
alias clear='for i in $(seq 25) ; do echo ; done && clear'

alias indent='indent -kr -ci2 -cli2 -i2 -l80 -nut'

alias j='TZ=Europe/Warsaw j'
alias ren='TZ=Europe/Warsaw ren'

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
	[ $PWD = $HOME ] && [ $# -eq 0 ] && local HIDDEN=false

	if ( $MSHELL_GNU ) ; then
		$MSHELL_LS --color --group-directories-first -lh $($HIDDEN && echo "-A") "$@" | less -S -R -F -X
	elif [ $MSHELL_SYS = 'macosx' ] ; then
		$MSHELL_LS -lhG $($HIDDEN && echo "-A") "$@"
	elif [ $MSHELL_SYS = 'windows' ] ; then
		$MSHELL_LS --color -lh $($HIDDEN && echo "-A") "$@"
	elif [ $MSHELL_SYS = 'bsd' ] ; then
		$MSHELL_LS -lhG $($HIDDEN && echo "-A") "$@"
	fi
}
