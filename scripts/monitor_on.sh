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
           "Monitor On" \
           "$1"
}

xrandr --output eDP-1  --mode 1368x768 --left-of HDMI-1 --noprimary
xrandr --output HDMI-1 --auto          --right-of eDP-1 --primary

notify_print "eDP-1 set to on as nonprimary"
