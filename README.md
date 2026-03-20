# semacs

A batteries-included Emacs configuration. Literate org-mode source, byte-compiled for fast startup, with built-in AI coding assistant integration ([opencode](https://opencode.ai) + [claude-code](https://github.com/yuya373/claude-code-emacs)).

<img width="2560" height="1440" alt="semacs screenshot" src="https://github.com/user-attachments/assets/4c25fd67-f41a-4527-a24f-0f1ec485b6bc" />

## Install

```sh
curl -fsSL https://raw.githubusercontent.com/sevapru/semacs/main/get.sh | bash
```

Then add your API keys:

```sh
$EDITOR ~/.emacs.d/.env
```

Start Emacs. That's it.

## Requirements

- Emacs 28+
- Git
- `apt` / `pacman` / `brew` (for system deps)

The installer handles everything else: system libraries, Emacs packages, byte-compilation, and AI tool setup.

## What's included

| Area | Package(s) |
|---|---|
| Completion | `vertico`, `corfu`, `cape`, `orderless`, `marginalia` |
| Navigation | `consult`, `embark`, `avy`, `projectile` |
| Git | `magit`, `diff-hl` |
| LSP | `eglot` |
| Terminal | `vterm` |
| AI | `claude-code`, `opencode` (vterm TUI) |
| Spell check | `jinx` |
| Theme | `solarized-theme`, `doom-modeline`, `nerd-icons` |
| Editing | `vundo`, `wgrep`, `which-key`, `beacon` |

## AI tools

### Claude Code (`C-c C-a`)

`claude-code` runs project-isolated Claude Code sessions in vterm:

```
C-c C-a         open transient menu
M-x claude-code-run   start a session
```

Requires the [Claude Code CLI](https://claude.ai/code) installed to `~/.local/bin/claude`.

### Opencode (`M-x opencode`)

`opencode` launches the opencode TUI in a dedicated vterm buffer. Config lives in `opencode/` and is symlinked to `~/.config/opencode/` at install time.

## Credentials

API keys go in `~/.emacs.d/.env` — gitignored, copy from vault:

```sh
# ~/.emacs.d/.env
ANTHROPIC_API_KEY=sk-ant-...
SOBAKA_API_KEY=sk-...

# Optional: override calendar location (defaults: Amsterdam)
# CALENDAR_LATITUDE=52.36547
# CALENDAR_LONGITUDE=4.81926
# CALENDAR_LOCATION=Amsterdam
# CALENDAR_TZ_OFFSET=60
```

Emacs loads `.env` at startup and injects vars into its process environment.

## Updating

```sh
cd ~/.emacs.d
git pull
make all
```

## Makefile

```
make all              # install packages + compile (default)
make compile          # tangle configuration.org → byte-compile
make tangle           # tangle only
make setup-opencode   # re-create ~/.config/opencode/ symlinks
make test             # benchmark startup time
make clean            # remove generated .el/.elc files
```

## Structure

```
.emacs.d/
├── init.el              # bootstrap loader
├── configuration.org    # literate config (source of truth)
├── get.sh               # curl installer
├── install.sh           # fresh-install script
├── Makefile             # build system
├── .env.example         # credentials template
├── opencode/            # opencode config (symlinked to ~/.config/opencode/)
│   ├── opencode.json
│   ├── AGENTS.md
│   └── skills/
└── lisp/                # local elisp packages
    └── org-block-extra.el
```

## License

MIT
