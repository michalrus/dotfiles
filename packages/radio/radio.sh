#!/usr/bin/env bash

set -euo pipefail

user_agent='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36'
mpv_mute_script='@mpvMuteScript@'
stations_yaml='@stationsYaml@'

get_station_url() {
  local name="$1"

  # shellcheck disable=SC2016
  yq -r --arg station "$name" '.stations[] | select(.name == $station) | .url' "$stations_yaml"
}

get_station_infixes() {
  local name="$1"

  # shellcheck disable=SC2016
  yq -r --arg station "$name" '.stations[] | select(.name == $station) | (.mute_title_infixes // []) | join(",")' "$stations_yaml"
}

get_station_mute_empty_title() {
  local name="$1"

  # shellcheck disable=SC2016
  yq -r --arg station "$name" '.stations[] | select(.name == $station) | (.mute_empty_title // false)' "$stations_yaml"
}

play_stream() {
  local name="$1"
  local url="$2"
  local infixes="${3:-}"
  local mute_empty_title="${4:-false}"
  local mpv_args=(
    --user-agent="$user_agent"
    --no-ytdl
    --no-resume-playback
    --script="$mpv_mute_script"
  )

  if [ -n "$infixes" ]; then
    mpv_args+=(--script-opts="radio-mute-title_infixes=$infixes")
  fi

  if [ "$mute_empty_title" = "true" ]; then
    mpv_args+=(--script-opts="radio-mute-mute_empty_title=yes")
  fi

  echo
  echo "Station: $name"
  echo "URL:     $url"
  echo

  exec mpv "${mpv_args[@]}" "$url"
}

mapfile -t stations_from_yaml < <(
  yq -r '.stations | sort_by(.name) | .[] | select(.name != "AccuRadio.com") | .name' "$stations_yaml"
)
stations=("AccuRadio.com" "${stations_from_yaml[@]}")

station=$(printf '%s\n' "${stations[@]}" | sk --no-sort)

if [ -z "${station}" ]; then
  exit 0
fi

case "$station" in
"AccuRadio.com")
  exec accuradio
  ;;
*)
  url=$(get_station_url "$station")
  infixes=$(get_station_infixes "$station")
  mute_empty_title=$(get_station_mute_empty_title "$station")
  if [ -z "$url" ]; then
    echo "No URL found for station: $station" >&2
    exit 1
  fi

  play_stream "$station" "$url" "$infixes" "$mute_empty_title"
  ;;
esac
