# -*- mode: shell-script; sh-shell: sh -*-

if [ -d "$HOME/.Xresources.d" ] ; then
  __xresources=$(mktemp)
  cat "$HOME/.Xresources.d"/* > "$__xresources"
  xrdb "$__xresources"
  rm "$__xresources"
  unset __xresources
fi

gpg-connect-agent /bye

git annex assistant --autostart --startdelay=120

feh --bg-scale "$HOME/.wallpaper.png" || true

# For ~/.XCompose to work in Gtk apps
export XMODIFIERS="@im=none"
export GTK_IM_MODULE=xim

export _JAVA_AWT_WM_NONREPARENTING=1

[ -e "$HOME/.xsession.local" ] && . "$HOME/.xsession.local"

exec bspwm
