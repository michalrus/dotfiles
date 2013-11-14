umask 077

alias mshell='curl -L -o "${HOME}/.mshell.tgz" "https://michalrus.com/mshell" && tar -xzvf "${HOME}/.mshell.tgz" --no-same-owner -C "${HOME}/"'

if [ -e /etc/bash_completion ] ; then
	. /etc/bash_completion

	HAVE_GIT_PS='yes'
	export GIT_PS1_SHOWDIRTYSTATE=1
	export GIT_PS1_SHOWSTASHSTATE=1
	export GIT_PS1_SHOWUNTRACKEDFILES=1
	export GIT_PS1_SHOWUPSTREAM='verbose'
else
	HAVE_GIT_PS='no'
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

if [ -d "${HOME}/bin" ] ; then
	export PATH="${HOME}/bin:${PATH}"
fi

if [ -d "/usr/local/bin" ] ; then
	export PATH="${PATH}:/usr/local/bin"
fi

export EDITOR='nano'
export VISUAL='nano'

if [[ ${EUID} == 0 ]] ; then
	PS1='\[\033[01;35m\]\u\[\033[01;31m\]@\[\033[01;35m\]\h\[\033[31;01m\]:\[\033[01;35m\]\w$(my_git_ps 31 35)\[\033[31;01m\]\$\[\033[00m\] '
else
	PS1='\[\033[01;36m\]\u\[\033[01;34m\]@\[\033[01;36m\]\h\[\033[34;01m\]:\[\033[01;36m\]\w$(my_git_ps 34 36)\[\033[34;01m\]\$\[\033[00m\] '
fi

SYSTEM='?'
GNU='no'

if ( uname | grep -i darwin >/dev/null ); then
	SYSTEM='macosx'

	if ( which gls >/dev/null ) ; then
		GNU='yes'
	fi
elif ( uname | grep -i bsd >/dev/null ) ; then
	SYSTEM='bsd'
elif ( uname | grep -i mingw >/dev/null ) ; then
	SYSTEM='windows'
else
	GNU='yes'
fi

if [ "$GNU" == 'yes' ] ; then
	eval "$(dircolors "${HOME}/.dircolors")"
	if [ "$SYSTEM" == 'macosx' ] ; then
		LS='gls'
	else
		LS='ls'
	fi
	alias d="${LS} --color --group-directories-first -lhA"
elif [ "$SYSTEM" == 'macosx' ] ; then
	export CLICOLOR='1'
	alias d='ls -lhAG'
elif [ "$SYSTEM" == 'windows' ] ; then
	alias d='ls --color -lhA'
elif [ "$SYSTEM" == 'bsd' ] ; then
	alias d='ls -lhAG'
fi

alias s='exec screen -d -r'
alias nano='nano -UwT 4'
alias nchmod='chmod -R u=rwX,g=rX,o=rX'
alias pchmod='chmod -R u=rwX,g=,o='
alias clear='for i in $(seq 25) ; do echo ; done && clear'

alias indent='indent -kr -ci2 -cli2 -i2 -l80 -nut'

alias j='TZ=Europe/Warsaw j'
alias ren='TZ=Europe/Warsaw ren'
