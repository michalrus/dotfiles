#!/usr/bin/env bash

set -euo pipefail

prog="${0##*/}"
data_dir="${XDG_DATA_HOME:-$HOME/.local/share}/qemu-win10"
system_image="$data_dir/system.qcow2"
shared_dir="$data_dir/shared"
guest_iso_default="@virtioWinIso@"
share_tag="hostshare"
spice_addr="127.0.0.1"
spice_port="${QEMU_WIN10_SPICE_PORT:-5900}"
spice_viewer="remote-viewer"

usage() {
  cat <<EOF
usage:
  $prog --install-base [--iso <iso> ...]
  $prog -a, --apps <name> [--iso <iso> ...]
  $prog --apps-rw <name> [--iso <iso> ...]
  $prog --prune-overlays
  $prog --unsafe-base-rw [--iso <iso> ...]
  $prog -l, --list-apps
  $prog -h, --help

options:
  -m, --mem <size>   Set RAM size (default: 4G)
  --iso <file>       Attach an ISO image (repeatable)
  --share-dir <dir>  Override the shared host directory

warning:
  --unsafe-base-rw boots system.qcow2 read-write for base setup only; using it later breaks apps-*.qcow2 images

Images live under: $data_dir
Shared folder: $shared_dir (mount tag: $share_tag)
EOF
}

die() {
  echo >&2 "$prog: $*"
  exit 1
}

require_spice_viewer() {
  command -v "$spice_viewer" >/dev/null 2>&1 || die "missing $spice_viewer in PATH"
}

spice_port_in_use() {
  if (exec 3<>"/dev/tcp/$spice_addr/$spice_port") 2>/dev/null; then
    exec 3<&- 3>&-
    return 0
  fi
  return 1
}

wait_for_spice() {
  local attempts=100
  while [ "$attempts" -gt 0 ]; do
    if (exec 3<>"/dev/tcp/$spice_addr/$spice_port") 2>/dev/null; then
      exec 3<&- 3>&-
      return 0
    fi
    attempts=$((attempts - 1))
    sleep 0.05
  done
  return 1
}

mode=""
apps_name=""
install_base="false"
isos=()

set_mode() {
  local next_mode="$1"
  if [ -n "$mode" ] && [ "$mode" != "$next_mode" ]; then
    die "conflicting options: $mode vs $next_mode"
  fi
  mode="$next_mode"
}

set_apps_name() {
  local next_name="$1"
  if [ -n "$apps_name" ]; then
    die "duplicate apps selection"
  fi
  apps_name="$next_name"
}

if ! parsed_args=$(getopt -o a:hlm: --long help,mem:,install-base,unsafe-base-rw,apps:,apps-rw:,iso:,share-dir:,list-apps,prune-overlays -- "$@"); then
  usage
  exit 2
fi
eval set -- "$parsed_args"

while true; do
  case "$1" in
  --install-base)
    install_base="true"
    shift 1
    ;;
  --unsafe-base-rw)
    set_mode "unsafe-base-rw"
    shift 1
    ;;
  --apps | -a)
    set_mode "apps"
    set_apps_name "$2"
    shift 2
    ;;
  --apps-rw)
    set_mode "apps-rw"
    set_apps_name "$2"
    shift 2
    ;;
  --mem | -m)
    mem="$2"
    shift 2
    ;;
  --prune-overlays)
    set_mode "prune-overlays"
    shift 1
    ;;
  --iso)
    isos+=("$2")
    shift 2
    ;;
  --share-dir)
    shared_dir="$2"
    shift 2
    ;;
  --list-apps | -l)
    set_mode "list-apps"
    shift 1
    ;;
  --help | -h)
    usage
    exit 0
    ;;
  --)
    shift
    break
    ;;
  *)
    usage
    die "unknown option: $1"
    ;;
  esac
done

if [ "$install_base" = "true" ]; then
  if [ -n "$mode" ]; then
    usage
    die "--install-base cannot be combined with $mode"
  fi
  mode="install-base"
fi

if [ -z "$mode" ]; then
  usage
  exit 2
fi

[ -n "$shared_dir" ] || die "--share-dir requires a path"

for iso_path in "${isos[@]}"; do
  [ -n "$iso_path" ] || die "--iso requires a path"
  [ -f "$iso_path" ] || die "ISO not found: $iso_path"
done

case "$mode" in
prune-overlays)
  if [ -d "$data_dir" ]; then
    rm -f -- "$data_dir"/overlay-*.qcow2
  fi
  exit 0
  ;;
list-apps)
  if [ ! -d "$data_dir" ]; then
    exit 0
  fi
  shopt -s nullglob
  files=("$data_dir"/apps-*.qcow2)
  shopt -u nullglob
  if [ "${#files[@]}" -eq 0 ]; then
    exit 0
  fi
  names=()
  for f in "${files[@]}"; do
    base="${f##*/}"
    base="${base#apps-}"
    base="${base%.qcow2}"
    names+=("$base")
  done
  printf '%s\n' "${names[@]}" | sort -u
  exit 0
  ;;
esac

mkdir -p "$data_dir" "$shared_dir"

mem="${mem:-4G}"
cpus="4"
# TODO: detect host CPU features and adjust Hyper-V flags for portability.
cpu_flags="host,hv_relaxed=on,hv_vapic=on,hv_spinlocks=0x1fff,hv_vpindex=on,hv_runtime=on,hv_time=on,hv_synic=on,hv_stimer=on,hv_frequencies=on,hv_tlbflush=on,hv_ipi=on,hv_evmcs=on,hv_avic=on"
guest_iso="$guest_iso_default"
system_size="80G"
qxl_ram_mb="512"
qxl_vram_mb="256"
qxl_vgamem_mb="64"

[ -n "$mem" ] || die "--mem requires a value"

if [ "$mode" = "install-base" ]; then
  if [ -e "$system_image" ]; then
    die "system.qcow2 already exists; remove $system_image to reinstall"
  fi
  [ -f "$guest_iso" ] || die "virtio drivers ISO not found: $guest_iso"
  qemu-img create -f qcow2 "$system_image" "$system_size"
  disk_image="$system_image"
elif [ "$mode" = "unsafe-base-rw" ]; then
  [ -f "$system_image" ] || die "missing $system_image; run --install-base first"
  [ -f "$guest_iso" ] || die "virtio drivers ISO not found: $guest_iso"
  disk_image="$system_image"
elif [ "$mode" = "apps" ] || [ "$mode" = "apps-rw" ]; then
  [ -n "$apps_name" ] || die "${mode} requires an apps name"
  apps_image="$data_dir/apps-$apps_name.qcow2"
  [ -f "$system_image" ] || die "missing $system_image; run --install-base first"
  if [ "$mode" = "apps-rw" ] && [ ! -f "$apps_image" ]; then
    qemu-img create -f qcow2 -b "$system_image" -F qcow2 "$apps_image"
  fi
  if [ "$mode" = "apps" ]; then
    [ -f "$apps_image" ] || die "missing $apps_image; use --apps-rw to create it"
    overlay_stamp="$(date -Ins)"
    overlay_stamp="${overlay_stamp//[^0-9A-Za-zT]/-}"
    overlay_image="$data_dir/overlay-$overlay_stamp.qcow2"
    qemu-img create -f qcow2 -b "$apps_image" -F qcow2 "$overlay_image"
    disk_image="$overlay_image"
  else
    disk_image="$apps_image"
  fi
else
  die "unsupported mode: $mode"
fi

qemu_args=(
  -enable-kvm
  -machine q35
  -cpu "$cpu_flags"
  -m "$mem"
  -smp "$cpus"
  -rtc base=localtime
  -vga none
  -display none
  -device "qxl-vga,ram_size_mb=${qxl_ram_mb},vram_size_mb=${qxl_vram_mb},vgamem_mb=${qxl_vgamem_mb}"
  -device "virtio-serial-pci,id=virtio-serial0"
  -chardev "spicevmc,id=spicechannel0,name=vdagent"
  -device "virtserialport,bus=virtio-serial0.0,nr=1,chardev=spicechannel0,name=com.redhat.spice.0"
  -usb
  -device usb-tablet
  -device virtio-rng-pci
  -device virtio-balloon-pci
  -audiodev "spice,id=audio0"
  -device ich9-intel-hda
  -device "hda-duplex,audiodev=audio0"
  -spice "port=$spice_port,addr=$spice_addr,disable-ticketing=on,image-compression=off,seamless-migration=on"
  -nic none
  -boot menu=on
  -virtfs "local,path=$shared_dir,mount_tag=$share_tag,security_model=none"
  -drive "file=$disk_image,if=virtio,format=qcow2,cache=writeback,discard=unmap"
)

if [ "$mode" = "install-base" ] || [ "$mode" = "unsafe-base-rw" ]; then
  qemu_args+=(
    -drive "file=$guest_iso,media=cdrom,readonly=on"
  )
fi

for iso_path in "${isos[@]}"; do
  qemu_args+=(
    -drive "file=$iso_path,media=cdrom,readonly=on"
  )
done

require_spice_viewer
if spice_port_in_use; then
  die "SPICE port ${spice_addr}:${spice_port} is already in use"
fi

qemu-system-x86_64 "${qemu_args[@]}" &
qemu_pid=$!
cleanup_done="false"

cleanup() {
  if [ "$cleanup_done" = "true" ]; then
    return
  fi
  cleanup_done="true"
  local status=0

  if [ -n "${qemu_pid:-}" ] && kill -0 "$qemu_pid" 2>/dev/null; then
    kill "$qemu_pid" 2>/dev/null || true
  fi
  if [ -n "${qemu_pid:-}" ]; then
    if ! wait "$qemu_pid" 2>/dev/null; then
      status=$?
    else
      status=0
    fi
    qemu_pid=""
  fi

  exit "$status"
}

trap cleanup EXIT INT TERM

if wait_for_spice; then
  "$spice_viewer" "spice://${spice_addr}:${spice_port}"
else
  echo >&2 "$prog: timed out waiting for SPICE on ${spice_addr}:${spice_port}"
fi
