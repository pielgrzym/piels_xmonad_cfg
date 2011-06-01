#!/bin/bash
if [ "$(xrandr -q | grep "DVI1 connected")" ]; then
        if [ "$(xrandr -q | grep "DVI1 connected 1920x1080")" ]; then
            xrandr --output DVI1 --off
            nitrogen --restore
        else
            xrandr --output DVI1 --mode 1920x1080 --right-of  LVDS1
            nitrogen --restore
        fi
else
    logger "$(xrandr -q | grep "DVI1 connected")"
fi
