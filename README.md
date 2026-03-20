# semacs

My Emacs. Literate config via `configuration.org`, compiled for fast startup, with opencode TUI integration.

<img width="2560" height="1440" alt="image" src="https://github.com/user-attachments/assets/4c25fd67-f41a-4527-a24f-0f1ec485b6bc" />

## Fresh Install

```bash
git clone git@github.com:sevapru/semacs.git ~/.emacs.d
cd ~/.emacs.d
bash install.sh
```

The script:
1. Installs system deps (`libvterm-dev`, `cmake`, `libenchant-2-dev`)
2. Creates `~/.emacs.d/.env` from the template (fill in your API keys)
3. Symlinks `opencode/` → `~/.config/opencode/` (config, skills, AGENTS.md)
4. Installs the opencode binary (`~/.opencode/bin/opencode`)
5. Installs all Emacs packages and byte-compiles the config

After install, fill in credentials:

```bash
$EDITOR ~/.emacs.d/.env
```

Then start Emacs. Run opencode with `M-x opencode`.

## Credentials (.env)

API keys live in `~/.emacs.d/.env` — gitignored, store it in your vault.

```bash
# .env (copy from .env.example, never commit)
ANTHROPIC_API_KEY=sk-ant-...
SOBAKA_API_KEY=sk-...
```

Emacs loads these into its process environment at startup so opencode and shell commands pick them up automatically.

## Opencode

The `opencode/` directory is versioned in this repo and symlinked to `~/.config/opencode/`:

| File | Purpose |
|---|---|
| `opencode/opencode.json` | Provider config (sobaka provider, `$SOBAKA_API_KEY`) |
| `opencode/AGENTS.md` | Global agent rules (identity, code style) |
| `opencode/skills/*.md` | Domain skills (business, robotics, infra, etc.) |

To re-run the symlink setup: `make setup-opencode`

## New Machine (snoek etc.)

```bash
git clone git@github.com:sevapru/semacs.git ~/.emacs.d
cd ~/.emacs.d
bash install.sh
# paste credentials from vault into ~/.emacs.d/.env
```

## Makefile

```
make all              # install packages + compile (default)
make install-packages # install Emacs packages from MELPA/ELPA
make tangle           # tangle configuration.org → configuration.el
make compile          # tangle + byte-compile
make setup-opencode   # re-create ~/.config/opencode/ symlinks
make test             # benchmark startup time
make clean            # remove generated .el/.elc files
```

## Structure

```
.emacs.d/
├── init.el              # bootstrap: loads compiled config or tangles on-the-fly
├── configuration.org    # literate config (source of truth)
├── install.sh           # fresh-install script
├── Makefile             # build system
├── .env.example         # credentials template (committed)
├── .env                 # real credentials (gitignored, from vault)
├── opencode/            # versioned opencode config
│   ├── opencode.json    # provider config ($SOBAKA_API_KEY)
│   ├── AGENTS.md        # global agent rules
│   └── skills/          # domain skill files
└── lisp/                # local elisp packages
    └── org-block-extra.el
```

Have a good day,
Seva
