#!/bin/bash

# path
PATH=\
$HOME/android-sdk-linux/tools:\
$HOME/android-sdk-linux/platform-tools:\
$HOME/.cabal/bin:\
$HOME/local/bin:\
/usr/local/symlinks:/usr/local/scripts:/usr/local/buildtools/java/jdk/bin:/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:\
$HOME/projects/depot_tools

export PATH

export PROMPT_COMMAND="history -a"

# xterm titlebar
case $TERM in
    xterm* | rxvt* | cygwin*)
        PROMPT_COMMAND='history -a; echo -ne "\033]0;$(hostname -s):$(basename $PWD)\007"'
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
alias ec="emacsclient -t" # yay for multi-tty!

# limits
ulimit -S -c 0

# history-related
shopt -s cmdhist
shopt -s checkwinsize
shopt -s cdable_vars
shopt -s histappend

alias h=history
alias r='fc -s'
#export HISTSIZE=10000
export HISTFILESIZE=100000
unset HISTSIZE

# term setup
stty stop ''

# exports
export EDITOR="emacsclient -a '' -t"

# google
export P4CONFIG=.p4config
