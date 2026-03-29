{
  flake,
  pkgs,
  lib,
  ...
}: let
  inherit (flake.packages.${pkgs.stdenv.hostPlatform.system}) yt-dlp;
  # Streaming only audio:
  mpva = pkgs.writeShellApplication {
    name = "mpva";
    text = ''exec mpv --no-resume-playback --no-video --ytdl-format='bestaudio[ext=m4a]/bestaudio' --ytdl-raw-options='ignore-config=' "$@"'';
  };
  # The Android player is often treated better, though with lower kb/s:
  mpva-android = pkgs.writeShellApplication {
    name = "mpva-android";
    text = ''exec mpv --no-resume-playback --no-video --ytdl-format='bestaudio/best' --ytdl-raw-options='ignore-config=,extractor-args=youtube:player_client=android' "$@"'';
  };
  uoscEnable = true;
in {
  programs.mpv = {
    enable = true;
    scripts = with pkgs.mpvScripts;
      [
        mpris
        thumbfast
        autosub
        easycrop
        acompressor
      ]
      ++ (lib.optionals uoscEnable [uosc]);
  };

  home = {
    packages = [
      yt-dlp
      pkgs.python3Packages.subliminal
      mpva
      mpva-android
    ];

    file =
      {
        ".config/mpv/input.conf".text = ''
          MBTN_LEFT  cycle pause
          ${lib.optionalString uoscEnable ''
            MBTN_RIGHT script-binding uosc/menu
          ''}

          WHEEL_DOWN add volume -2
          WHEEL_UP   add volume  2
          UP         add volume  2
          DOWN       add volume -2

          LEFT        seek  -5
          RIGHT       seek   5
          SHIFT+LEFT  seek -60
          SHIFT+RIGHT seek  60

          SHIFT+z  add sub-font-size -1                                   #! Subtitles > Decrease font size
          SHIFT+x  add sub-font-size +1                                   #! Subtitles > Increase font size
          CTRL+z   add sub-pos +1                                         #! Subtitles > Move down
          CTRL+x   add sub-pos -1                                         #! Subtitles > Move up

          # By default it just toggles them on and off, but let's cycle, since one of the items is empty:
          v        cycle sid                                               #! Subtitles > Cycle track

          # <https://www.miejski.pl/slowo-Filmy+z+%C5%BC%C3%B3%C5%82tymi+napisami>
          SHIFT+s  cycle-values sub-color "#ffff00" "#ffffff"              #! Subtitles > Toggle yellow

          b        script-binding autosub/download_subs                    #! Subtitles > Download (subliminal)
          n        script-binding autosub/download_subs2                   #! Subtitles > Download 2nd language (subliminal)
          ${lib.optionalString uoscEnable ''
            #        script-binding uosc/download-subtitles                  #! Subtitles > Search Open Subtitles
            #        script-binding uosc/load-subtitles                      #! Subtitles > Load from file
          ''}

          # For remote control:
          FORWARD multiply speed 1.1
          REWIND  multiply speed 1/1.1
        '';

        ".config/mpv/mpv.conf".text = ''
          ${
            if uoscEnable
            then ''
              # Required by uosc (and thumbfast integration therein):
              osc=no
              osd-bar=no
            ''
            else ''
              # Required by the patched `osc.lua` with `thumbfast` support:
              osc=no
              # Match OSC tick rate to display refresh (uosc does this by default):
              script-opts-append=osc-tick_delay_follow_display_fps=yes
            ''
          }

          volume-max=400.0
          audio-channels=stereo
          osd-fractions=yes
          osd-duration=1500
          save-position-on-quit=yes
          sub-font-size=44
          sub-codepage=cp1250
          sub-auto=all
          sub-margin-y=0
          sub-pos=98
          script-opts=ytdl_hook-ytdl_path=${lib.getExe yt-dlp}

          # Smooth stream start without stuttering:
          cache-pause-initial=yes
          cache-pause-wait=15
        '';

        ".config/yt-dlp/config".text = ''
          --embed-metadata
          --remux-video mp4
        '';
      }
      // (lib.optionalAttrs (!uoscEnable) {
        # Stock mpv `osc.lua` patched with ~60 lines of thumbfast integration. The
        # upstream `po5/thumbfast` `vanilla-osc` branch is based on a ~2019
        # `osc.lua` and misses 5+ years of improvements, so we patch the current one:
        ".config/mpv/scripts/osc.lua".source = pkgs.runCommandLocal "osc-thumbfast.lua" {} ''
          ${lib.getExe pkgs.rsync} -R ${pkgs.mpv-unwrapped.src}/./player/lua/osc.lua .
          chmod -R +w .
          ${lib.getExe pkgs.patch} -p1 < ${./thumbfast-osc.patch}
          cp player/lua/osc.lua $out
        '';
      });

    # For CLI completion:
    shellAliases.youtube-dl = "yt-dlp";
  };
}
