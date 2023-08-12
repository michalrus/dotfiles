{ hiDPI ? false }: ''

#define S_base03        #002b36
#define S_base02        #073642
#define S_base01        #586e75
#define S_base00        #657b83
#define S_base0         #839496
#define S_base1         #93a1a1
#define S_base2         #eee8d5
#define S_base3         #fdf6e3
#define S_yellow        #b58900
#define S_orange        #cb4b16
#define S_red           #dc322f
#define S_magenta       #d33682
#define S_violet        #6c71c4
#define S_blue          #268bd2
#define S_cyan          #2aa198
#define S_green         #859900
! Solarized dark
Emacs.background:  S_base03
Emacs.foreground:  S_base1
Emacs.fadeColor:   S_base03
Emacs.cursorColor: S_base1
Emacs.pointerColor: S_base1
Emacs.pointerColorBackground: S_base01
Emacs.pointerColorForeground: S_base1
Emacs.menuBar:            off
Emacs.toolBar:            off
Emacs.verticalScrollBars: off
Emacs.font:               Iosevka:style=Regular:size=32
Emacs.font-1:             unifont:fontformat=truetype:size=32:antialias=true
Emacs.font-2:             FontAwesome:style=Regular:size=32

Xcursor.theme: Adwaita
Xcursor.size: 48
XTerm*metaSendsEscape: true
XTerm*utf8: 1
XTerm*saveLines: 0
XTerm*cursorBlink: true
XTerm*selectToClipboard: true
XTerm*charClass: 33:48,36-47:48,58-59:48,61:48,63-64:48,95:48,126:48
XTerm*faceName: Iosevka:style=Regular:size=12
XTerm*visualbell: true
XTerm*bellIsUrgent: true
XTerm*fullscreen: never
XTerm*borderWidth: 0

XTerm*translations: #override \
  Shift <Key>Insert:    insert-selection(SELECT) \n\
  Ctrl <Key>Insert:     copy-selection(SELECT)

XTerm*background:  S_base03
XTerm*foreground:  S_base1
XTerm*color0: S_base02
XTerm*color8: S_base02
XTerm*color1: S_red
XTerm*color9: S_orange
XTerm*color2: S_green
XTerm*color10: S_green
XTerm*color3: S_yellow
XTerm*color11: S_yellow
XTerm*color4: S_blue
XTerm*color12: S_blue
XTerm*color5: S_magenta
XTerm*color13: S_magenta
XTerm*color6: S_cyan
XTerm*color14: S_cyan
XTerm*color7: S_base1
XTerm*color15: S_base3

''
