#!/usr/bin/env zsh

DISPLAY=''

rsvg-convert -w "$2" -h "$2" "$1" > "$3"
