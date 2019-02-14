#!/bin/bash

# path
PATH=\
$HOME/go/bin:\
/usr/local/go/bin:\
$HOME/.depot_tools/depot_tools:\
$ANDROID_HOME/tools:\
$ANDROID_HOME/platform-tools:\
$HOME/.cabal/bin:\
$HOME/.local/bin:\
/usr/local/scripts:\
/usr/local/buildtools/java/jdk/bin:\
/snap/bin:\
/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin:\
$HOME/projects/depot_tools

export PATH

export PROMPT_COMMAND="history -a"

# xterm titlebar
case $TERM in
    xterm* | rxvt* | cygwin*)
        PROMPT_COMMAND='history -a; echo -ne "\033]0;$(hostname -s):$(basename "$PWD")\007"'
      ;;
    *)
	;;
esac


SSH_STATUS=""
if [ -n "$SSH_CLIENT" ]; then
    SSH_STATUS=" (ssh)"
fi

# prompts
case $TERM in
    dumb* | emacs* )
	export PS1="[\w]\$ "
	;;
    *)
	export PS1="\[\033[01;32m\]\n[\w] \j (\u@\h)$SSH_STATUS\n\$ \[\033[00m\]"
	export PS2="\w$ "
	export PS3="\$ "
	;;
esac

EMACSCLIENT="emacsclient"
if [ -e "/Applications/Emacs.app/Contents/MacOS/bin/emacsclient" ]; then
    EMACSCLIENT="/Applications/Emacs.app/Contents/MacOS/bin/emacsclient"
fi

# aliases
alias ec="$EMACSCLIENT -t" # yay for multi-tty!

# limits
ulimit -S -c 0

# history-related
shopt -s cmdhist
shopt -s checkwinsize
shopt -s cdable_vars
shopt -s histappend

alias h=history

export HISTFILESIZE=100000
unset HISTSIZE

# term setup
stty stop ''

# exports
export EDITOR="$EMACSCLIENT -a '' -t"

case $TERM in
  xterm* | rxvt* | screen* )
    export PAGER="less"
esac

# print a message reminding me of tmux sessions
if [[ $- == *i* ]] && test '!' $TMUX; then
  tmux list-sessions
fi

# complete go commands if gotab is installed
which gotab >/dev/null && complete -C gotab -o nospace go

alias gohome="cd $HOME/go/src/github.com/PilotFiber/"
export GO111MODULE=on
export GOPROXY=https://athens.pilotfiber.ninja/

if [ -f /usr/local/etc/bash_completion ]; then
    source /usr/local/etc/bash_completion;
fi
