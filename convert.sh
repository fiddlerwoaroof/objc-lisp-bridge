#!/usr/bin/env zsh

DISPLAY=''

/Applications/Inkscape.app/Contents/Resources/bin/inkscape --export-png "$(realpath "$3")" -w $2 -h $2 "$(realpath "$1")"
