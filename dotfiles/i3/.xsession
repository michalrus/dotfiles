# -*- mode: shell-script; sh-shell: sh -*-

joined=$(mktemp)
find "$HOME/.xsession.d" -not -name '*.example' -print0 | sort -z | xargs -r0 cat > "$joined"
. "$joined"
