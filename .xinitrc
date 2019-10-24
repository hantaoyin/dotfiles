#!/bin/bash -
#
# ~/.xinitrc
#
# Executed by startx (run your window manager from here)

# exec gnome-session
# exec startkde
# exec startxfce4
# ...or the Window Manager of your choice

#xscreensaver -no-splash &
qiv -x ~/images/wallpaper.jpg &
fcitx &

# Disable caps lock altogether.
# setxkbmap -option caps:none

# caps lock -> control
setxkbmap -option ctrl:nocaps

xset fp+ ~/.fonts

# Turn off (blank) monitor after 300 seconds.  For details about this, see
# https://wiki.archlinux.org/index.php/Display_Power_Management_Signaling
# xset s 300 300

#amixer set Master 80% unmute
#amixer set PCM 100% unmute
#amixer set Front 70% unmute
#amixer set Headphone mute
xset b off
# xsetroot -cursor_name left_ptr

# while true; do
#     xsetroot -name "$(LC_ALL=C date +"%a %F %R")"
#     sleep 60
# done &
# exec dwm

#exec enlightenment_start
#exec openbox-session
#exec fvwm
#exec wmii
#exec openbox-session
exec xmonad
