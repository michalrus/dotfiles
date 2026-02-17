#!/usr/bin/env bash

set -euo pipefail

user_agent='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36'
stations_url='https://stream.chillhop.com/stations'

fetch_stream_list() {
  local stations_json
  local stream_list

  if ! stations_json=$(curl -A "$user_agent" -fsSL "$stations_url"); then
    echo >&2 "warning: failed to fetch stations from $stations_url"
    return 1
  fi

  if [ -z "$stations_json" ]; then
    echo >&2 "warning: empty stations JSON from $stations_url"
    return 1
  fi

  if ! stream_list=$(jq -r '
    .stations[]?
    | select(.id != null)
    | [
        .id,
        (.name // "")
      ]
    | @tsv
  ' <<<"$stations_json"); then
    echo >&2 "warning: failed to parse stations JSON from $stations_url"
    return 1
  fi

  if [ -z "$stream_list" ]; then
    echo >&2 "warning: no stations found in $stations_url"
    return 1
  fi

  printf '%s\n' "$stream_list"
}

choose_stream() {
  if ! command -v sk >/dev/null 2>&1; then
    echo >&2 'fatal: sk not found'
    return 1
  fi

  local stream_list
  local selection

  if ! stream_list=$(fetch_stream_list); then
    return 1
  fi

  selection=$(printf '%s\n' "$stream_list" | sk --no-sort --delimiter $'\t' --with-nth 2 || true)
  if [ -z "$selection" ]; then
    return 1
  fi

  printf '%s' "$selection"
}

if ! selection=$(choose_stream); then
  echo >&2 'fatal: no stream selected'
  exit 1
fi

stream_id="${selection%%$'\t'*}"
stream_name="${selection#*$'\t'}"
api_url="https://stream.chillhop.com/live/${stream_id}"

echo
echo "Station: $stream_name"
echo "URL:     $api_url"

while true; do
  playlist_json=$(curl -A "$user_agent" -fsSL "$api_url")

  if [ -z "$playlist_json" ]; then
    echo >&2 "fatal: empty playlist JSON from $api_url"
    exit 1
  fi

  track_list=$(jq -r '
    .[]
    | select(.streamUrl != null and .streamUrl != "")
    | [
        .streamUrl,
        (.title // ""),
        (.artists // "")
      ]
    | @tsv
  ' <<<"$playlist_json")

  if [ -z "$track_list" ]; then
    echo >&2 "fatal: no playable tracks in $api_url"
    exit 1
  fi

  while IFS=$'\t' read -r stream_url title artists; do
    if [ -z "$stream_url" ]; then
      continue
    fi

    echo
    echo "Track:   $title"
    echo "Artist:  $artists"
    echo "URL:     $stream_url"
    echo

    mpv \
      --user-agent="$user_agent" \
      --no-ytdl \
      --no-resume-playback \
      "$stream_url"
  done <<<"$track_list"
done
