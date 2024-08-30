{ flake, config, pkgs, ... }:

{
  fonts.enableDefaultPackages = false;  # DejaVu fonts break Noto Sans Emoji fallback

  fonts.packages = with pkgs; [
    iosevka-bin
    (nerdfonts.override {fonts = ["Iosevka" "NerdFontsSymbolsOnly"];})
    noto-fonts
    noto-fonts-emoji
    noto-fonts-cjk
    #google-fonts  # They break ‘noto-fonts-emoji’
    liberation_ttf
  ];

  fonts.enableGhostscriptFonts = true;

  fonts.fontconfig.defaultFonts = {
    monospace = ["Noto Sans Mono"];
    sansSerif = ["Noto Sans"];
    serif     = ["Noto Serif"];
    emoji     = ["Noto Color Emoji"];
  };

  fonts.fontconfig.localConf = ''
    <?xml version="1.0"?>
    <!DOCTYPE fontconfig SYSTEM "urn:fontconfig:fonts.dtd">
    <fontconfig>
      <match>
        <edit mode="append" name="family"><string>Noto Color Emoji</string></edit>
      </match>
      <alias>
        <family>for_terminal</family>
        <prefer>
          <family>Iosevka Term Light</family>
          <family>Symbols Nerd Font</family>
          <family>Noto Color Emoji</family>
        </prefer>
      </alias>
      <alias>
        <family>for_editor</family>
        <prefer>
          <family>Iosevka NFP Light</family>
          <family>Noto Color Emoji</family>
        </prefer>
      </alias>
    </fontconfig>
  '';
}
