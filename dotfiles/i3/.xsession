# -*- mode: shell-script; sh-shell: sh -*-

# For ~/.XCompose to work in Gtk apps
export XMODIFIERS="@im=none"
export GTK_IM_MODULE=xim

export _JAVA_AWT_WM_NONREPARENTING=1

if [ -d "$HOME/.Xresources.d" ] ; then
  __xresources=$(mktemp)
  cat "$HOME/.Xresources.d"/* > "$__xresources"
  xrdb "$__xresources"
  rm "$__xresources"
  unset __xresources
fi

amixer -q sset Master 0 && amixer -q sset Master mute

gpg-connect-agent /bye

git annex assistant --autostart --startdelay=120

feh --bg-scale "$HOME/.wallpaper.png" || true

compton &

xbanish &

nm-applet &

dunst &

space2ctrl &

systemctl --user start screen.service &
systemctl --user start emacs-daemon.service &

[ -e "$HOME/.xsession.local" ] && . "$HOME/.xsession.local"

. "$HOME/.xsession.wm"
