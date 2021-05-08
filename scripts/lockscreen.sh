#!/bin/bash

declare LOCKSCREEN_FILE=/tmp/lockscreen.png

scrot $LOCKSCREEN_FILE

convert \
	-blur 0x8 \
	$LOCKSCREEN_FILE \
	$LOCKSCREEN_FILE
convert \
	-gravity SouthWest \
	-composite $LOCKSCREEN_FILE $HOME/.xmonad/res/RnM_lockscreen_shadow.png \
	-gravity South \
	$LOCKSCREEN_FILE

feh -ZYFxN $LOCKSCREEN_FILE &
FEH_PID=$!
xmonadctl dunst-pause

XLPASSWD=trustnobody /home/jonathas/Repositories/xl/xl

xmonadctl dunst-resume
kill -TERM $FEH_PID
rm $LOCKSCREEN_FILE
