#!/bin/sh
if [ "$SCREENLAYOUT" == "right-vertical" ]
then
  source ~/dotfiles/.screenlayout/horizontal.sh
elif [ "$SCREENLAYOUT" == "horizontal" ]
then
  source ~/dotfiles/.screenlayout/right-vertical.sh
else
  echo "Error: $SCREENLAYOUT not set properly"
fi
