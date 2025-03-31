#!/bin/bash

# path
PATH=\
$HOME/bin:\
$HOME/go/bin:\
$HOME/.krew/bin:\
$HOME/.local/bin:\
$HOME/.dotfiles/bin:\
/usr/local/scripts:\
/sbin:\
/usr/sbin:\
$PATH

export PATH

source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

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
[[ -r "/opt/homebrew/etc/profile.d/bash_completion.sh" ]] && . "/opt/homebrew/etc/profile.d/bash_completion.sh"

alias cover="go test -coverprofile=cover.out -covermode=atomic"
alias coverall="go test -coverprofile=cover.out ./... -covermode=atomic -coverpkg=./..."
alias coverreport="go tool cover -html cover.out -o cover.html && serveme cover.html"
alias kctx=kubectx
alias kns=kubens
alias fl="fly -t jrock"
complete -F _complete_alias fl
alias k="kubectl"
complete -F _complete_alias k
complete -F _completion_goflags jlog

export XCURSOR_SIZE=16
export SSH_AUTH_SOCK=$(find /tmp/ssh-* -path '*/ssh-*' -name 'agent*' -uid $(id -u) 2>/dev/null| tail -n1)

alias pctx="pachctl config set active-context"
alias gaz="bazel run //:gazelle"
alias bdf="bazel run //:buildifier"
alias master="jj bookmark move --to=@ master"
alias main="jj bookmark move --to=@ main"
