# bwrap-escape-hatch

Controlled sandbox escape for Bubblewrap. A sandboxed process connects
to a UNIX socket on the host, sends a command as a JSON argv array, and
gets back an exit code. An allow-list of fnmatch rules decides what may
run. systemd socket-activation starts a one-shot handler per connection;
nothing runs when idle.

## Wire protocol

One UNIX-stream connection = one request.

**Client** writes a JSON array (the argv) then closes the write half:

```json
["notify-send", "--", "Build finished", "All tests passed"]
```

**Service** reads (up to 64 KiB), matches against the allow-list, runs
the command (or rejects), and writes back:

```json
{ "ok": true, "exit_code": 0 }
```

or `{ "ok": false, "error": "no matching rule" }`, then closes.

## Allow-list matching

Rules live in a JSON file (see `rules.json` for an example). Each rule
has an `argv` array of fnmatch(3) patterns matched positionally against
the request. `FNM_PATHNAME` is **off**, so `*` matches `/` — we're
matching opaque argument strings, not filesystem paths.

A request matches if the lengths are equal and every element matches its
pattern. First match wins.

Arguments containing `..` path components are rejected unconditionally
before matching, preventing traversal attacks on path-prefix rules.

## Layout

- `src/main.rs` — the service binary (~180 lines)
- `rules.json` — default allow-list
- `default.nix` — Nix package + per-command guest shim generator
- `hm-module.nix` — Home Manager module (systemd socket + service)
