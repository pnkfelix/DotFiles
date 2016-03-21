#!/bin/sh

userresources=$HOME/.Xresources
usermodmap=$HOME/.Xmodmap
sysresources=/etc/X11/xinit/.Xresources
sysmodmap=/etc/X11/xinit/.Xmodmap

# merge in defaults and keymaps

if [ -f $sysresources ]; then
    xrdb -merge $sysresources
fi

if [ -f $sysmodmap ]; then
    xmodmap $sysmodmap
fi

if [ -f "$userresources" ]; then
    xrdb -merge "$userresources"
fi

if [ -f "$usermodmap" ]; then
    xmodmap "$usermodmap"
fi

# Swap left-ctrl and capslock
~/bin/maybe_swap_caps.sh

# start some nice programs

if [ -d /etc/X11/xinit/xinitrc.d ] ; then
 for f in /etc/X11/xinit/xinitrc.d/?*.sh ; do
  [ -x "$f" ] && . "$f"
 done
 unset f
fi

# exec enlightenment_start

# Keep i3 as the default
session=${2:-i3}

date >> /tmp/xinitrc.log
echo session: $session >> /tmp/xinitrc.log

case $session in
    i3|i3wm     ) exec i3;;
    # No known session, try to run it as command
    *)            exec $session;;
esac

# exec i3
