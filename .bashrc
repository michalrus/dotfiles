. ~/.mshell-common

if [ -e /etc/bash_completion ] ; then
	. /etc/bash_completion
fi

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
