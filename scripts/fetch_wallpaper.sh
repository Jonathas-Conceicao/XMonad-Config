#! /bin/bash

set -e

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
           "Fetch Wallpaper" \
           "$1"
}

notify_print "Fetching image"

POSTDATA=$(wget -qO- 'https://www.reddit.com/r/wallpapers/top.json?limit=1&t=week'| jq -r .data.children[-1].data)
POSTDATA_URL=$(echo $POSTDATA | jq -r .url)

echo $POSTDATA_URL

if [[ $POSTDATA_URL == *"reddit.com/gallery/"* ]]; then
  BASEURL=https://i.redd.it/$(echo $POSTDATA | jq -r .gallery_data.items[0].media_id)
  wget $BASEURL.jpg -O /tmp/wallpaper || wget $BASEURL.png -O /tmp/wallpaper
else
  wget $POSTDATA_URL -O /tmp/wallpaper
fi

POST_AUTHOR="$(echo $POSTDATA | jq -r .author)"
POST_TITLE="$(echo $POSTDATA | jq -r .title)"
POST_UPS="$(echo $POSTDATA | jq -r .ups)"

notify_print "$POST_TITLE by u/$POST_AUTHOR\nPosted on r/wallpapers with $POST_UPS ups"

feh --bg-scale /tmp/wallpaper
