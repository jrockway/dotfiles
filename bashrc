#!/bin/bash

# path
PATH=\
$HOME/utils/android/tools:\
$HOME/.cabal/bin:\
$HOME/projects/jrockway-utils/bin:\
$HOME/utils:\
/usr/local/bin:\
/usr/local/sbin:\
/usr/games:\
/usr/bin:\
/usr/X11R6/bin:\
/sbin:\
/usr/sbin:\
/bin

export PATH

# xterm titlebar
case $TERM in
    xterm* | rxvt* | cygwin*)
        PROMPT_COMMAND='echo -ne "\033]0;$(whoami)@$(hostname):${PWD} [$(jobs -s | wc -l | sed -e "s/ //g")]\007"'
      ;;
    *)
	;;
esac

# prompts
case $TERM in
    dumb* | emacs* )
	export PS1="[\w]\$ "
	;;
    *)
	export PS1="\[\033[01;32m\]\n[\w] \j (\u@\h)\n\$ \[\033[00m\]"
	export PS2="\w$ "
	export PS3="\$ "
	;;
esac

# aliases
alias xmms=nyxmms2

function svup {
    svc -u "$HOME/.dotfiles/services/$1"
    svstat "$HOME/.dotfiles/services/$1"
}

function svdn {
    svc -d "$HOME/.dotfiles/services/$1"
    svstat "$HOME/.dotfiles/services/$1"
}

function svst {
    svstat "$HOME/.dotfiles/services/$1"
}

alias perlfunc="perldoc -f"
alias lperl="perl -Ilib"
function lbperl {
    perl -Ilib "bin/$1";
}

alias ec="emacsclient -t" # yay for multi-tty!

# limits
ulimit -c 0

# history-related
shopt -s cmdhist
shopt -s checkwinsize
shopt -s cdable_vars
alias h=history
alias r='fc -s'
#export HISTSIZE=10000
export HISTFILESIZE=100000
unset HISTSIZE

# term setup
stty stop ''

# exports
export EDITOR="mg"
alias edit="$EDITOR"

alias cs="perl script/*_server.pl -d"
alias carpcs="perl -MCarp::Always script/*_server.pl -d"

alias prepl="pclient +PC --name=repl -PC"

eval $(perl -Mlocal::lib)

if [ -f /etc/bash_completion ]; then
    . /etc/bash_completion
fi
