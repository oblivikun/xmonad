#!/bin/bash

# System tray
if [ -z "$(pgrep trayer)" ] ; then
    trayer --edge bottom \
           --align right \
           --widthtype percent \
           --height 24 \
           --alpha 80 \
           --transparent true \
           --width 5 \
           --tint 0x282c34 &
fi

# Power manager
if [ -z "$(pgrep xfce4-power-manager)" ] ; then
    xfce4-power-manager &
fi

# Redshift
if [ -z "$(pgrep redshift)" ] ; then
    redshift &
fi
if [ -z "$(pgrep conky)"] ; then
  conky -c /home/erel/.config/conky/Shelyak-Light/Shelyak-Light.conf
fi

# Wallpaper
if [ -z "$(pgrep nitrogen)" ] ; then
    feh --bg-scale /home/erel/Pictures/wallpapers/gentoo-10-years-of-compiling-red-1920x1200.jpg
fi

# Screensaver
if [ -z "$(pgrep xscreensaver)" ] ; then
    xscreensaver -no-splash &
fi

#picom
picom -b &
# Network Applet
if [ -z "$(pgrep nm-applet)" ] ; then
    nm-applet &
fi

# Google Drive
if [ -z "$(pgrep insync)" ] ; then
    insync start &
fi
pulseaudio -D

flameshot &
stty sane & 
xbindkeys &
echo "done"
# xbindkeys
