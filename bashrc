#!/bin/bash

# path
PATH=\
$HOME/pach/install:\
$HOME/bin:\
$HOME/go/bin:\
$HOME/.fly/bin:\
$HOME/.cargo/bin:\
$HOME/.npm-packages/bin:\
$HOME/.krew/bin:\
/snap/bin:\
$HOME/.local/bin:\
$HOME/.gem/bin:\
$HOME/.gem/ruby/2.5.0/bin:\
$HOME/.dotfiles/bin:\
/usr/local/scripts:\
$PATH

export PATH

if [ -x /opt/homebrew/bin/brew ]; then
   eval "$(/opt/homebrew/bin/brew shellenv)"
   export SHELL=`which bash`
   if [ '!' -z ${TMUX+x} ]; then
       export TERMINFO=/opt/homebrew/Cellar/ncurses/6.3/share/terminfo/74/tmux-direct
   fi
fi

if [ -x /home/linuxbrew/.linuxbrew/bin/brew ]; then
    eval "$(/home/linuxbrew/.linuxbrew/bin/brew shellenv)"
    export NVM_DIR="$HOME/.nvm"
    [ -s "/home/linuxbrew/.linuxbrew/opt/nvm/nvm.sh" ] && \. "/home/linuxbrew/.linuxbrew/opt/nvm/nvm.sh"
    [ -s "/home/linuxbrew/.linuxbrew/opt/nvm/etc/bash_completion.d/nvm" ] && \. "/home/linuxbrew/.linuxbrew/opt/nvm/etc/bash_completion.d/nvm"
fi

# Nix
if [ -e '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh' ]; then
  . '/nix/var/nix/profiles/default/etc/profile.d/nix-daemon.sh'
fi
# End Nix

# Use /usr/local/go instead of homebrew's.
PATH=/usr/local/go/bin:$PATH

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

if [ -f $HOME/.env.pach ]; then
    source $HOME/.env.pach
fi

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
