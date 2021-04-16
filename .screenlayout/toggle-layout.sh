#!/usr/bin/env sh

SCREENLAYOUT=$(<~/dotfiles/.screenlayout/current-layout)

if [ $SCREENLAYOUT == "right-vertical" ]
then
	sh ~/dotfiles/.screenlayout/horizontal.sh
elif [ $SCREENLAYOUT == "horizontal" ]
then
	sh ~/dotfiles/.screenlayout/right-vertical.sh
else
	echo "Error: ./current-layout not formatted properly"
	sh ~/dotfiles/.screenlayout/right-vertical.sh
fi
