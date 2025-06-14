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
           "Disconnected from Bluetooth Headset" \
           "$1"
}

HEADSET_KNOWN_DEVICES=( "Edifier Headset W800BT"  "60:F4:3A:A2:44:67"
                        "Kuba Disco"  "C4:6E:7B:2D:0E:BC"
                      )

for (( i=0; $i < ${#HEADSET_KNOWN_DEVICES[@]}; i+=2 )); do
  name=${HEADSET_KNOWN_DEVICES[i+0]}
  dev=${HEADSET_KNOWN_DEVICES[i+1]}

  bluetoothctl disconnect $dev
done

notify_print "Disconnected for all known headset devices"
