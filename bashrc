#!/bin/bash

# path
PATH=\
$HOME/perl/install/bin:\
$HOME/.cabal/bin:\
$HOME/haskell/bin:\
$HOME/projects/jrockway-utils/bin:\
$HOME/utils:\
/var/qmail/bin:\
/usr/local/bin:\
/usr/local/sbin:\
/sw/bin:\
/sw/sbin:\
/opt/bin:\
/opt/sbin:\
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
alias xmms=xmms2
alias xm_intel="xmms2 config alsa.device intel; xmms2 stop"
alias xm_usb="xmms2 config alsa.device usb; xmms2 stop"


alias perlfunc="perldoc -f"
alias lperl="perl -Ilib"
function lbperl {
    perl -Ilib "bin/$1";
}

alias gap="git add --patch"
function mod {
    cd ~/projects/cpan_modules/$1*
}

alias ec="emacsclient -t" # yay for multi-tty!

wlan="wlan0"

function net {
    sudo iwconfig $wlan ap any;
    sudo iwconfig $wlan essid "$1";
    sleep 1;
    sudo dhclient $wlan;
}

function phone {
    sudo ifconfig $wlan down
    sudo iwconfig $wlan mode ad-hoc
    sudo ifconfig $wlan up
    sudo iwconfig $wlan essid WMWifiRouter
    sleep 1;
    sudo dhclient $wlan
}

function phone_down {
    sudo ifconfig $wlan down
    sudo iwconfig $wlan mode managed
    sudo ifconfig $wlan up
    sudo iwconfig $wlan essid "none"
}

function asdf_install {
    sbcl --eval "(asdf:operate 'asdf:load-op :asdf-install)" --eval "(asdf-install:install :$1)" --eval "(quit)"
}

function asdf_oos {
    rlwrap sbcl --eval "(asdf:operate 'asdf:$2 :$1)"
}

# limits
ulimit -c 0

# history-related
shopt -s cmdhist
shopt -s checkwinsize
shopt -s cdable_vars
alias h=history
alias r='fc -s'
export HISTSIZE=10000
export HISTFILESIZE=10000

# term setup
stty stop ''

# exports
#export CFLAGS="-O0 -g3"
export EDITOR="mg"
#export EDITOR="emacs -nw --no-desktop "
alias edit="$EDITOR"
export SVKMERGE="svk-merge-emacs"

alias cs="perl script/*_server.pl -d"
alias carpcs="perl -MCarp::Always script/*_server.pl -d"
unset HISTSIZE
