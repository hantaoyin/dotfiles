#!/bin/bash -
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice


#exec enlightenment_start
#exec openbox-session
#exec fvwm
#exec wmii

#xscreensaver -no-splash &
# qiv --root ~/media/wallpaper.jpg &
qiv -x ~/images/wallpaper.jpg &
xmodmap ~/.xmodmap
xset fp+ ~/.fonts

#amixer set Master 80% unmute
#amixer set PCM 100% unmute
#amixer set Front 70% unmute
#amixer set Headphone mute
xset b off
xsetroot -cursor_name left_ptr

# while true; do
#     xsetroot -name "$(LC_ALL=C date +"%a %F %R")"
#     sleep 60
# done &
# exec dwm

#exec openbox-session

exec xmonad
# exec dwm
