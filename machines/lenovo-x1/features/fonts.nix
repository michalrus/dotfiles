{ flake, config, pkgs, ... }:

{
  fonts.enableDefaultFonts = false;  # DejaVu fonts break Noto Sans Emoji fallback

  fonts.fonts = with pkgs; [
    (nerdfonts.override {fonts = ["Iosevka"];})
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk
    #google-fonts  # They break ‘noto-fonts-emoji’
  ];

  fonts.enableGhostscriptFonts = true;

  fonts.fontconfig.defaultFonts = {
    monospace = ["Iosevka NFM" "Noto Color Emoji"];
    sansSerif = ["Noto Sans"   "Noto Color Emoji"];
    serif     = ["Noto Serif"  "Noto Color Emoji"];
    emoji     = ["Noto Color Emoji"];
  };
}
