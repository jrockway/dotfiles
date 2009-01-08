#!/bin/bash

cd $HOME;

export TZ="America/Chicago"

scim -d
urxvtd -q -f -o
xmodmap ~/.xmodmaprc
xbindkeys

pidof emacs || emacs --daemon &
unclutter -idle 1 &

offlineimap &

. $HOME/utils/irc-ssh.sh

exec xmonad | xmobar ~/.xmonad/mobar.conf
