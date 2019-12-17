#!/bin/bash

# path
PATH=\
$HOME/bin:\
$HOME/go/bin:\
/usr/local/go/bin:\
$HOME/.npm-packages/bin:\
/snap/bin:\
$HOME/.local/bin:\
$HOME/.dotfiles/bin:\
/usr/local/scripts:\
/usr/local/sbin:/usr/local/bin:/usr/sbin:/usr/bin:/sbin:/bin

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

if [ -f /etc/bash_completion ]; then
    source /etc/bash_completion;
    for i in $HOME/.dotfiles/completion/*; do source $i; done
fi

alias json="npx prettier --stdin --stdin-filepath foo.json"
alias cover="go test ./... -p 1 -covermode=count -coverprofile cover.out && go tool cover -html cover.out; rm -f cover.out"
alias kctx=kubectx
alias kns=kubens
alias fl="fly -t jrock"
complete -F _complete_alias fl
alias blaze=bazel
complete -F _complete_alias blaze
