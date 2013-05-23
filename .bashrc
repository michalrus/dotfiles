umask 077

BASHRC_DIR="$(readlink -f "$(dirname "${BASH_SOURCE[0]}")")"

if [ -d "${BASHRC_DIR}/bin" ] ; then
	export PATH="${BASHRC_DIR}/bin:${PATH}"
fi

export EDITOR="nano"
export VISUAL="nano"

eval "$(dircolors "${BASHRC_DIR}/.dircolors")"
alias d="ls --color --group-directories-first -lhA"

if ( uname | grep -i freebsd >/dev/null ) ; then
	alias d="ls -lhAG"
fi

alias clear='for i in $(seq 25) ; do echo ; done && clear'
alias j="TZ=Europe/Warsaw j"
alias ren="TZ=Europe/Warsaw ren"
alias h="history 27"
alias s="exec screen -d -r"
alias nano="nano -UwT 4"
alias nchmod="chmod -R u=rwX,g=rX,o=rX"
alias pchmod="chmod -R u=rwX,g=,o="

alias mshell="wget -O ~/.mshell.tgz http://michalrus.com/mshell.tgz && tar -xzvf ~/.mshell.tgz -C ~/"

if [[ ${EUID} == 0 ]] ; then
	PS1='\[\033[01;35m\]\u\[\033[01;31m\]@\[\033[01;35m\]\h\[\033[31;01m\]:\[\033[01;35m\]\w\[\033[31;01m\]\$\[\033[00m\] '
else
	PS1='\[\033[01;36m\]\u\[\033[01;34m\]@\[\033[01;36m\]\h\[\033[34;01m\]:\[\033[01;36m\]\w\[\033[34;01m\]\$\[\033[00m\] '
fi
