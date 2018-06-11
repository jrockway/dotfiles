#!/bin/bash

ANDROID_HOME="/usr/local/jrockway/android-sdk-linux"
export ANDROID_HOME

# path
PATH=\
$HOME/.depot_tools/depot_tools:\
$ANDROID_HOME/tools:\
$ANDROID_HOME/platform-tools:\
$HOME/.cabal/bin:\
$HOME/.local/bin:\
/usr/local/scripts:\
/usr/local/buildtools/java/jdk/bin:\
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
alias g="cd /usr/local/google/code"

#export HISTSIZE=10000
export HISTFILESIZE=100000
unset HISTSIZE

# term setup
stty stop ''

# exports
export EDITOR="emacsclient -a '' -t"
export _JAVA_AWT_WM_NONREPARENTING=1

# go
export GOPATH="$HOME/projects/go"
export PATH="$HOME/projects/go/bin:$PATH"

# google
export P4CONFIG=.p4config
export P4DIFF="diff -u"
export PAGER="cat"
case $TERM in
  xterm* | rxvt* | screen* )
    export PAGER="less"
esac

if [ -e $HOME/.google-bashrc ]; then
    source $HOME/.google-bashrc
fi

if [ -e $HOME/.gpg-agent-info ]; then
    source $HOME/.gpg-agent-info
    export GPG_AGENT_INFO
    export SSH_AUTH_SOCK
    export SSH_AGENT_PID
fi

# for chromeos
umask 022

# print a message reminding me of tmux sessions
if test '!' $TMUX; then
  tmux list-sessions
fi
