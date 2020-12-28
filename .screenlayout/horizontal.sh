#!/bin/sh

xrandr --output DP-1 --off --output DP-2 --off --output DP-3 --mode 1920x1080 --pos 1920x0 --rotate normal --output HDMI-1 --primary --mode 1920x1080 --pos 0x0 --rotate normal

xsetbg -at 0,0 ~/dotfiles/.screenlayout/1080x1920-bang.jpg -at 1920,0 ~/dotfiles/.screenlayout/1080x1920-bang.jpg

echo "horizontal" > ./current-layout
