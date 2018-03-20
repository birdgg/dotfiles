#!/bin/bash

xsetroot -cursor_name left_ptr
xrdb ~/.Xresources
export GTK_IM_MODULE=fcitx
export QT_IM_MODULE=fcitx
export XMODIFIERS=@im=fcitx
eval "$(dbus-launch --sh-syntax --exit-with-session)"
exec xmonad
