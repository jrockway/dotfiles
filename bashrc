#!/bin/bash

# path
PATH=\
/usr/local/lib/nodejs/node-v10.15.3-linux-x64/bin:\
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
	export PS2="\$ "
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

# exports
export EDITOR="$EMACSCLIENT -a '' -t"

case $TERM in
  xterm* | rxvt* | screen* )
    export PAGER="less"
esac

# stuff to be done only in interactive shells
if [[ $- == *i* ]] && test '!' $TMUX; then
  stty stop ''

  # print a message reminding me of tmux sessions
  tmux list-sessions
fi

# complete go commands if gotab is installed
which gotab >/dev/null && complete -C gotab -o nospace go

alias gohome="cd $HOME/go/src/github.com/PilotFiber/"
export GO111MODULE=on
export GOPROXY=https://athens.pilotfiber.ninja/

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion;
    if [ -f $HOME/.dotfiles/kubectl_completion ]; then
        source $HOME/.dotfiles/kubectl_completion;
    fi
fi

alias json="npx prettier --stdin --stdin-filepath foo.json"
alias kloc="kubectl config use-context minikube"
alias kprod="kubectl config use-context arn:aws:eks:us-east-1:393453136086:cluster/production-kube"
