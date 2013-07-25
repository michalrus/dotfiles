umask 077

alias mshell='curl -L -o "${HOME}/.mshell.tgz" "https://michalrus.com/mshell" && tar -xzvf "${HOME}/.mshell.tgz" -C "${HOME}/"'

if [ -d "${HOME}/bin" ] ; then
	export PATH="${HOME}/bin:${PATH}"
fi

export EDITOR='nano'
export VISUAL='nano'

if [[ ${EUID} == 0 ]] ; then
	PS1='\[\033[01;35m\]\u\[\033[01;31m\]@\[\033[01;35m\]\h\[\033[31;01m\]:\[\033[01;35m\]\w\[\033[31;01m\]\$\[\033[00m\] '
else
	PS1='\[\033[01;36m\]\u\[\033[01;34m\]@\[\033[01;36m\]\h\[\033[34;01m\]:\[\033[01;36m\]\w\[\033[34;01m\]\$\[\033[00m\] '
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
else
	GNU='yes'
fi

if [ "$GNU" == 'yes' ] ; then
	eval "$(dircolors "${HOME}/.dircolors")"
	alias d="ls --color --group-directories-first -lhA"
elif [ "$SYSTEM" == 'macosx' ] ; then
	export CLICOLOR='1'
	alias d='ls -lhAG'
elif [ "$SYSTEM" == 'bsd' ] ; then
	alias d='ls -lhAG'
fi

alias s='exec screen -d -r'
alias nano='nano -UwT 4'
alias nchmod='chmod -R u=rwX,g=rX,o=rX'
alias pchmod='chmod -R u=rwX,g=,o='
alias clear='for i in $(seq 25) ; do echo ; done && clear'

alias j='TZ=Europe/Warsaw j'
alias ren='TZ=Europe/Warsaw ren'
