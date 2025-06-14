#! /bin/bash

declare APP_NAME=$(basename $0)
# Get an notification ID from script's name using
# numeric part of sha1sum modded to 10k to avoid overflow
declare NOTIFY_ID=$(echo $APP_NAME | sha1sum | sed 's|[[:alpha:]]||g' | awk '{print $1 % 10000;}')

function notify_print() {
  if [ $# -ne 1 ]; then
    exit 33
  fi

  dunstify --urgency=low \
           --appname=$APP_NAME \
           --replace=$NOTIFY_ID \
           "Minitor Off" \
           "$1"
}

if [[ $(xrandr | grep 'HDMI-1 connected') ]]; then
  xrandr --output eDP-1 --off
  notify_print "eDP-1 screen turned off"
else
  notify_print "HDMI-1 was not found, we avoid turning main screen off"
fi
