{ flake, lib, pkgs }: ''

set $mod Mod4

bindsym $mod+Shift+e exec "zenity --no-wrap --question --default-cancel --text 'Do you want to end your session?' && ( pkill -9 gpg-agent ; i3-msg exit ; )"

font pango:for_editor 10.5
new_window pixel 5
new_float normal

floating_modifier $mod

# Intercept Super+L, because it bleeds over from global actkbd’s config:
bindsym Mod4+l nop

bindsym $mod+d exec "rofi -combi-modi drun,run -show combi -modi combi"
bindsym $mod+e exec "${lib.getExe (flake.packages.${pkgs.system}.rofi-unicode-input.override { onlyEmoji = true; })}"
bindsym $mod+u exec "${lib.getExe  flake.packages.${pkgs.system}.rofi-unicode-input}"
bindsym $mod+Shift+q kill
bindsym $mod+i exec xrandr-invert-colors
bindsym Print exec ${lib.getExe flake.packages.${pkgs.system}.x11-screenshot}

bindsym XF86AudioMute         exec --no-startup-id "amixer -q sset Master 0 mute"
bindsym XF86AudioRaiseVolume  exec --no-startup-id amixer -q sset Master 3%+ unmute
bindsym XF86AudioLowerVolume  exec --no-startup-id amixer -q sset Master 3%- unmute
bindsym XF86MonBrightnessUp   exec --no-startup-id brightnessctl -d intel_backlight s 5%+
bindsym XF86MonBrightnessDown exec --no-startup-id brightnessctl -d intel_backlight s 5%-

bindsym F11 exec pkill -USR2 dunst
bindsym F12 exec pkill -USR1 dunst

bindsym $mod+x move workspace to output right

bindsym $mod+w layout toggle all
bindsym $mod+f fullscreen toggle
bindsym $mod+space floating toggle

focus_wrapping no

bindsym $mod+Left focus left
bindsym $mod+Down focus down
bindsym $mod+Up focus up
bindsym $mod+Right focus right

bindsym $mod+Shift+Left move left
bindsym $mod+Shift+Down move down
bindsym $mod+Shift+Up move up
bindsym $mod+Shift+Right move right

# bindsym $mod+a focus parent
# bindsym $mod+z focus child
# bindsym $mod+h split h
# bindsym $mod+v split v

bindsym $mod+1 workspace 1
bindsym $mod+2 workspace 2
bindsym $mod+3 workspace 3
bindsym $mod+4 workspace 4
bindsym $mod+5 workspace 5
bindsym $mod+6 workspace 6
bindsym $mod+7 workspace 7
bindsym $mod+8 workspace 8
bindsym $mod+9 workspace 9
bindsym $mod+0 workspace 10

bindsym $mod+Shift+1 move container to workspace 1
bindsym $mod+Shift+2 move container to workspace 2
bindsym $mod+Shift+3 move container to workspace 3
bindsym $mod+Shift+4 move container to workspace 4
bindsym $mod+Shift+5 move container to workspace 5
bindsym $mod+Shift+6 move container to workspace 6
bindsym $mod+Shift+7 move container to workspace 7
bindsym $mod+Shift+8 move container to workspace 8
bindsym $mod+Shift+9 move container to workspace 9
bindsym $mod+Shift+0 move container to workspace 10

bindsym $mod+minus scratchpad show
bindsym $mod+Shift+minus move scratchpad
for_window [class="^Peek$"] floating enable

mode "resize" {
  bindsym Left  resize shrink width  10 px or 5 ppt
  bindsym Down  resize grow   height 10 px or 5 ppt
  bindsym Up    resize shrink height 10 px or 5 ppt
  bindsym Right resize grow   width  10 px or 5 ppt

  bindsym $mod+r mode "default"
  bindsym Return mode "default"
  bindsym Escape mode "default"

  bindsym $mod+t floating enable; mode "move"
}

mode "move" {
  bindsym Up move up 20px
  bindsym Left move left 20px
  bindsym Down move down 20px
  bindsym Right move right 20px

  bindsym $mod+t mode "default"
  bindsym Return mode "default"
  bindsym Escape mode "default"

  bindsym $mod+r mode "resize"
}

bindsym $mod+r mode "resize"
bindsym $mod+t floating enable; mode "move"

# colorclass            border  backgr. text    indicator
client.focused          #0c5f73 #0c5f73 #eee8d5 #1c6497
client.focused_inactive #073642 #073642 #93a1a1 #073642
client.unfocused        #073642 #073642 #93a1a1 #073642
client.urgent           #dc322f #dc322f #eee8d5 #cb4b16

bar {
  status_command i3status
  tray_output primary # set it with xrandr --output <TAB> --primary
  separator_symbol " | "
  position top
  colors {
    background #002b36
    statusline #93a1a1
    separator #586e75
    # colorclass       border  backgr. text
    focused_workspace  #002b36 #268bd2 #eee8d5
    active_workspace   #002b36 #6c71c4 #eee8d5
    inactive_workspace #002b36 #073642 #586e75
    urgent_workspace   #002b36 #dc322f #eee8d5
    binding_mode       #002b36 #dc322f #eee8d5
  }
}

bindsym $mod+Return exec alacritty
bindsym $mod+Shift+Return exec emacs

exec ~/.config/autostart/dotfiles.sh

''
