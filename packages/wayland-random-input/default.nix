{
  lib,
  writeShellApplication,
  coreutils,
  fuzzel,
  wtype,
  wl-clipboard,
  util-linux,
  xxd,
  calc,
}: let
  options = [
    {
      label = "Random: UUID";
      command = ''${util-linux}/bin/uuidgen -r'';
    }
    {
      label = "Random: hex string";
      command = ''head -c 32 /dev/urandom | xxd -p -c 1024'';
    }
    {
      label = "Random: base32 string";
      command = ''head -c 128 /dev/urandom | base32 -w 0 | tr -d +/ | cut -c1-32'';
    }
    {
      label = "Random: base64 string";
      command = ''head -c 128 /dev/urandom | base64 -w 0 | tr -d +/ | cut -c1-32'';
    }
    {
      label = "Random: u8";
      command = ''calc 0x"$(head -c 1 /dev/urandom | xxd -p -c 1024)" | tr -d '\t' '';
    }
    {
      label = "Random: u16";
      command = ''calc 0x"$(head -c 2 /dev/urandom | xxd -p -c 1024)" | tr -d '\t' '';
    }
    {
      label = "Random: u32";
      command = ''calc 0x"$(head -c 4 /dev/urandom | xxd -p -c 1024)" | tr -d '\t' '';
    }
    {
      label = "Random: u64";
      command = ''calc 0x"$(head -c 8 /dev/urandom | xxd -p -c 1024)" | tr -d '\t' '';
    }
    {
      label = "Random: u128";
      command = ''calc 0x"$(head -c 16 /dev/urandom | xxd -p -c 1024)" | tr -d '\t' '';
    }
    {
      label = "Random: coin flip";
      command = ''calc "0x$(head -c 16 /dev/urandom | xxd -p -c 256) % 2" | tr -d '\t' '';
    }
    {
      label = "Random: 1d6 die";
      command = ''calc "1 + 0x$(head -c 16 /dev/urandom | xxd -p -c 256) % 6" | tr -d '\t' '';
    }
    {
      label = "Random: percentage";
      command = ''calc "0x$(head -c 16 /dev/urandom | xxd -p -c 256) % 101" | tr -d '\t' '';
    }
  ];
in
  writeShellApplication {
    name = "wayland-random-input";
    runtimeInputs = [coreutils fuzzel wtype wl-clipboard xxd calc];
    text = ''
      selected=$(fuzzel <<<${lib.escapeShellArg (lib.concatMapStringsSep "\n" (a: a.label) options)} --dmenu --width=100)

      output=""
      case "$selected" in
      ${lib.concatMapStringsSep "\n" (option: ''
          ${lib.escapeShellArg option.label}) output=$(${option.command});;
        '')
        options}
      *) exit 1;;
      esac

      wtype "$output"
    '';
    derivationArgs.meta.description = "Write various random strings on Wayland";
    derivationArgs.meta.platforms = lib.platforms.linux;
  }
