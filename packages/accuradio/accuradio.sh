#!/usr/bin/env bash

set -euo pipefail

main_html=$(curl -fsSL https://www.accuradio.com/)
main_state=$(<<<"$main_html" grep -F 'window.__PRELOADED_STATE__' | sed -r 's|^.*window\.__PRELOADED_STATE__\s*=\s*(.*)</script>.*|\1|g')

if [ -z "$main_state" ] ; then
    echo >&2 'fatal: empty main_state'
    exit 1
fi

genre=$(<<<"$main_state" jq -r '.content.genres.brands | map(.name) | .[]' | sort | sk --no-sort)
genre_canonical=$(<<<"$main_state" jq --arg genre "$genre" -r '.content.genres.brands[] | select(.name == $genre) | ((.canonical_url | select(. != "")) // .param)')

genre_html=$(curl -fsSL "https://www.accuradio.com/${genre_canonical}/")
genre_state=$(<<<"$genre_html" grep -F 'window.__PRELOADED_STATE__' | sed -r 's|^.*window\.__PRELOADED_STATE__\s*=\s*(.*)</script>.*|\1|g')

if [ -z "$genre_state" ] ; then
    echo >&2 'fatal: empty genre_state'
    exit 1
fi

channel=$(<<<"$genre_state" jq -r '.content.genrePageData | (.channels + [.featuredChannel]) | .[] | (.name + "   \t" + (.description | gsub("\n"; "")))' | column -t -s $'\t' | sort | sk --no-sort | sed -r 's|^(.*)   .*$|\1|g ; s|\s*$||g')
channel_id=$(<<<"$genre_state" jq -r --arg channel "$channel" '.content.genrePageData | (.channels + [.featuredChannel]) | .[] | select(.name == $channel) | ._id["$oid"]')

while true ; do
    channel_json=$(curl -fsSL "https://www.accuradio.com/playlist/json/${channel_id}/")

    jq <<<"$channel_json" -c '.[] | select(.ad_type == null)' | while IFS= read -r track ; do
        track_url=$(jq -r '.primary + .fn + ".m4a"' <<<"$track")

        echo
        jq -r '("Track:   " + .title + "\nArtist:  " + .track_artist + "\nAlbum:   " + .album.title + "\nYear:    " + .album.year)' <<<"$track"
        echo "Genre:   $genre"
        echo "Channel: $channel"
        echo "URL:     $track_url"
        echo

        mpv --no-resume-playback "$track_url"
    done
done
