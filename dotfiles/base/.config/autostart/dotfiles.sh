#!/bin/sh

joined=$(mktemp)
find "$HOME/.config/autostart/dotfiles.d" -not -type d -not -name '*.example' -print0 | sort -z | xargs -r0 cat >"$joined"
. "$joined"
