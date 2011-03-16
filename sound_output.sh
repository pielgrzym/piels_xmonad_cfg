#!/bin/bash

if [ $1 == 0 ]
then
        # headphones
        amixer -c 0 -- sset PCM 50% > /dev/null 2>&1
        sleep 0.35
        amixer -c 0 -- sset Front unmute > /dev/null 2>&1
        amixer -c 0 -- sset Surround mute > /dev/null 2>&1
else
        # speakers
        amixer -c 0 -- sset Front mute > /dev/null 2>&1
        amixer -c 0 -- sset Surround unmute > /dev/null 2>&1
        amixer -c 0 -- sset PCM 100% > /dev/null 2>&1
fi
