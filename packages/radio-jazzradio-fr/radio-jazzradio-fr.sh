#!/usr/bin/env bash

set -euo pipefail

user_agent='Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/143.0.0.0 Safari/537.36'
webradio_url='https://www.jazzradio.fr/radio/webradio'

fetch_channel_list() {
  local html

  if ! html=$(curl -A "$user_agent" -fsSL "$webradio_url"); then
    echo >&2 "warning: failed to fetch $webradio_url"
    return 1
  fi

  if [ -z "$html" ]; then
    echo >&2 "warning: empty response from $webradio_url"
    return 1
  fi

  # Extract id/slug pairs from <a> links with rel="open_player" (deduplicated, order-preserving)
  local ids_slugs
  ids_slugs=$(printf '%s' "$html" |
    grep -oP 'href="https://www\.jazzradio\.fr/radio/webradio/\K[0-9]+/[^"]+(?="[^>]*rel="open_player")' |
    awk '!seen[$0]++')

  # Extract channel names from radio-name divs (only inside radio-item blocks)
  # Reformat "Jazz Radio Foo" -> "JazzRadio.fr: Foo" (or just "JazzRadio.fr" for the main channel)
  local names
  names=$(printf '%s' "$html" | grep -oP '<div class="radio-name">\K[^<]+' |
    sed -e 's/^Jazz Radio /JazzRadio.fr: /' -e 's/^Jazz Radio$/JazzRadio.fr/')

  if [ -z "$ids_slugs" ] || [ -z "$names" ]; then
    echo >&2 "warning: no channels found"
    return 1
  fi

  # Combine: id/slug<TAB>name
  local channel_list
  if ! channel_list=$(paste <(printf '%s\n' "$ids_slugs") <(printf '%s\n' "$names")); then
    echo >&2 "warning: failed to combine channel data"
    return 1
  fi

  if [ -z "$channel_list" ]; then
    echo >&2 "warning: no channels found after combining"
    return 1
  fi

  printf '%s\n' "$channel_list"
}

fetch_stream_url() {
  local channel_id="$1"
  local channel_slug="$2"
  local page_url="https://www.jazzradio.fr/radio/webradio/${channel_id}/${channel_slug}"
  local html
  local stream_url

  if ! html=$(curl -A "$user_agent" -fsSL "$page_url"); then
    echo >&2 "warning: failed to fetch $page_url"
    return 1
  fi

  # Extract from the "flux" input field on the channel page
  stream_url=$(printf '%s' "$html" | grep -oP 'name="flux"[^>]*value="\K[^"]+' || true)

  if [ -z "$stream_url" ]; then
    # Fallback: extract data-src from audio source element
    stream_url=$(printf '%s' "$html" | grep -oP 'data-src="\Khttps://[^"]*\.ice\.infomaniak\.ch/[^"?]+' | head -1 || true)
  fi

  if [ -z "$stream_url" ]; then
    echo >&2 "warning: could not find stream URL for channel $channel_id"
    return 1
  fi

  printf '%s' "$stream_url"
}

choose_channel() {
  local channel_list
  local selection

  if ! channel_list=$(fetch_channel_list); then
    return 1
  fi

  # Format: id/slug\tname -- sort by name, show only name to user
  selection=$(printf '%s\n' "$channel_list" | LC_ALL=C sort -t $'\t' -k2 | sk --no-sort --delimiter $'\t' --with-nth 2 || true)
  if [ -z "$selection" ]; then
    return 1
  fi

  printf '%s' "$selection"
}

if ! selection=$(choose_channel); then
  echo >&2 'fatal: no channel selected'
  exit 1
fi

# Format: id/slug\tname
id_slug="${selection%%$'\t'*}"
channel_name="${selection#*$'\t'}"
channel_id="${id_slug%%/*}"
channel_slug="${id_slug#*/}"

echo
echo "Station: $channel_name"

if ! stream_url=$(fetch_stream_url "$channel_id" "$channel_slug"); then
  echo >&2 "fatal: could not get stream URL for $channel_name"
  exit 1
fi

echo "URL:     $stream_url"
echo

retry_delay=2
max_retry_delay=60
while true; do
  start=$SECONDS
  rc=0
  mpv \
    --user-agent="$user_agent" \
    --no-ytdl \
    --no-resume-playback \
    --loop-file=inf \
    "$stream_url" || rc=$?
  # Exit on success (0) or signal quit (4)
  if ((rc == 0 || rc == 4)); then exit "$rc"; fi
  # Reset backoff if mpv ran for more than a minute (transient vs. immediate failure)
  if ((SECONDS - start > 60)); then
    retry_delay=2
  fi
  echo >&2 "mpv exited with error (code $rc), retrying in ${retry_delay}s..."
  sleep "$retry_delay"
  retry_delay=$((retry_delay * 2))
  if ((retry_delay > max_retry_delay)); then
    retry_delay=$max_retry_delay
  fi
done
