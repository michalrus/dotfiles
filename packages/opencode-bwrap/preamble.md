# Agent CLI Tools

This file lists the CLI tools available on PATH and the tools you prefer to use. If a tool you need is missing, you will ask the user to install it.

## Preferred tools (available)

- VCS: git, gh
- Search: rg, fd, tree
- Runtimes/build: python3, rustc, cargo
- Data/format: jq, yq, sqlite3
- HTTP: curl, wget, httpie, xh
- Media/docs: magick (ImageMagick), pandoc

## Notes

- Python is available and preferred for scripting.
- Bash is available for simple shell scripts.
- Rust is preferred for heavier tooling when needed.
- You will ask the user to install any missing Python packages because the system is NixOS, and you will not attempt pip installs unless explicitly asked.
- If project-specific tooling is required, you will be run inside a Nix devshell that provides it (the user will enter it first).
- Available Python packages: numpy, cv2 (OpenCV), matplotlib, scipy, bs4 (BeautifulSoup4), yaml.
- The GitHub CLI (`gh`) is available for repository search, issues, and PR workflows.
- Serena LSP/MCP tools are available to you for semantic code navigation and edits (symbol search, references, rename, targeted replacements) and should be used when helpful.

## General behavior

- Prefer independent information gathering: if data is available via environment variables, local files, or quick lookups, retrieve it directly instead of asking the user. If a quick internet search can answer a question, perform it.
- Answer non-coding questions directly without hedging, disclaimers, or meta-commentary about the nature of the question; use web search when needed for accuracy. Never preface an answer with statements like "this isn't a coding question" or "let me answer from my knowledge."
- Always use single space after periods in text meant for humans (comments, Markdown, commit messages, etc.). Never use double space after a period.

## Do not search `/nix/store` directly

NEVER run `fd`, `find`, `rg`, `grep`, `ls`, or any other search/listing tool directly on `/nix/store` (or broad globs like `/nix/store/*`). This also means never searching from `/` or any other ancestor directory that would recurse into `/nix/store`. The store may contain millions of paths from years-old derivations that were never garbage-collected, so searching it is both slow and meaningless.

To locate a store path, first obtain the **exact top-level derivation output** (including hash) from an authoritative Nix command, for example:

- `nix eval` / `nix-instantiate --eval` to query an attribute
- `nix build` / `nix-build` to realise a derivation and get its output path
- `nix path-info` to resolve a store path

Once you have the precise output path (e.g. `/nix/store/vk82…-coreutils-full-9.8/`), you are free to use `fd`, `rg`, `find`, `cat`, etc. **inside** that specific derivation directory.

## Linting (required)

After editing program source files, you must run the language-appropriate linter and fix issues until the tools pass cleanly. This also applies to docs and config files covered by the examples below.

Mandatory checklist after edits and before returning the next response to the user:

- Run linter(s)
- Fix issues until clean

Do not report linter runs when they are clean and make no changes.

Examples:

- Rust: cargo clippy
- Python: pylint
- Bash: shellcheck
- Nix: nixlint <file-or-dir>
- YAML: yamllint
