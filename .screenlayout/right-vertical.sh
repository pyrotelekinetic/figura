#!/bin/sh
xrandr --output DP-1 --off --output DP-2 --off --output DP-3 --mode 1920x1080 --pos 1920x0 --rotate left --output HDMI-1 --primary --mode 1920x1080 --pos 0x420 --rotate normal
export SCREENLAYOUT="right-vertical"
xsetbg -at 0,420 ./1080x1920-bang.jpg -at 1920,0 ./1920x1080-spikeandjet.png
