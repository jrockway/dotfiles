#!/bin/bash

cd $HOME
source "$HOME/.bashrc"

if [ -e "$HOME/.tz" ]; then
    read TZ < $HOME/.tz
else
    TZ="America/Chicago"
fi
export TZ

setxkbmap -option ctrl:nocaps
scim -d
xmodmap ~/.xmodmaprc
xbindkeys
unclutter -idle 1 &
xsetroot -solid black

trayer --edge top --align right --SetDockType true --SetPartialStrut true --expand true --width 15 --heighttype pixel --height 11  --transparent 1 --tint 0x000000 --alpha 0 --distancefrom right --distance 200 &
nm-applet &
gnome-power-manager &

# pidof svscan || (svscan /home/jon/.dotfiles/services 2>&1 | readproctitle jon ........................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................................) &

FIFO="$HOME/tmp/emacs.fifo"
rm $FIFO
mkfifo $FIFO

exec xmonad | xmobar ~/.xmonad/mobar.conf
