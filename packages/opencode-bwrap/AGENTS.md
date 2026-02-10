# Agent CLI Tools

This file lists the CLI tools available on PATH and the tools I prefer to use. If a tool I need is missing, I will ask the user to install it.

## Preferred tools (available)

- VCS: git
- Search: rg, fd, tree
- Runtimes/build: python3, rustc, cargo
- Data/format: jq, yq, sqlite3
- HTTP: curl, wget, httpie, xh
- Media/docs: ImageMagick, pandoc

## Notes

- Python is available and preferred for scripting.
- Bash is available for simple shell scripts.
- Rust is preferred for heavier tooling when needed.
- I will ask the user to install any missing Python packages because the system is NixOS, and I will not attempt pip installs unless explicitly asked.
- If project-specific tooling is required, I will be run inside a Nix devshell that provides it (the user will enter it first, then run `opencode` inside).
- Available Python packages: numpy, cv2 (OpenCV), matplotlib, scipy, bs4 (BeautifulSoup4).

## Linting and formatting (required)

After editing program source files, I must run the language-appropriate formatter and linter and fix issues until the tools pass cleanly. This also applies to docs and config files covered by the examples below.

Mandatory checklist after any edit:

- Run formatter(s)
- Run linter(s)
- Fix issues until clean

Do not report formatter/linter runs when they are clean and make no changes.

Examples:

- Rust: cargo fmt, cargo clippy
- Python: black, pylint
- Bash: shfmt, shellcheck
- Markdown/JSON: prettier (no linter)
- YAML: yamlfmt, yamllint

If the required tools are unavailable, I must say so and request their installation.
