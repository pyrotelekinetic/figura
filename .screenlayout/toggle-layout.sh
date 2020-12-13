#!/bin/sh
if [ "$SCREENLAYOUT" == "right-vertical" ]
then
  source ./horizontal.sh
elif [ "$SCREENLAYOUT" == "horizontal" ]
then
  source ./right-vertical.sh
else
  echo "Error: $SCREENLAYOUT not set properly"
fi
