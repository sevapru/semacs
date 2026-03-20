#!/usr/bin/env bash
# semacs — fresh install script
# Usage: bash install.sh [--no-opencode] [--no-system-deps]
set -euo pipefail

EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
OPENCODE_BIN="$HOME/.opencode/bin/opencode"
NO_OPENCODE=0
NO_SYSTEM_DEPS=0

for arg in "$@"; do
  case "$arg" in
    --no-opencode)     NO_OPENCODE=1 ;;
    --no-system-deps)  NO_SYSTEM_DEPS=1 ;;
  esac
done

log()  { echo "[semacs] $*"; }
warn() { echo "[semacs] WARNING: $*" >&2; }

# ── 1. System dependencies (libvterm for vterm package) ─────────────────────
if [[ "$NO_SYSTEM_DEPS" -eq 0 ]]; then
  if command -v apt-get &>/dev/null; then
    log "Installing system deps (libvterm-dev, cmake)..."
    sudo apt-get install -y libvterm-dev cmake
  elif command -v pacman &>/dev/null; then
    log "Installing system deps (libvterm, cmake)..."
    sudo pacman -S --noconfirm libvterm cmake
  elif command -v brew &>/dev/null; then
    log "Installing system deps (libvterm, cmake)..."
    brew install libvterm cmake
  else
    warn "Could not detect package manager. Install libvterm and cmake manually."
  fi
fi

# ── 2. .env credentials file ────────────────────────────────────────────────
if [[ ! -f "$EMACS_DIR/.env" ]]; then
  log "Creating .env from template..."
  cp "$EMACS_DIR/.env.example" "$EMACS_DIR/.env"
  log "  → Edit ~/.emacs.d/.env and fill in your API keys (from vault)."
else
  log ".env already exists, skipping."
fi

# ── 3. opencode global config symlinks ──────────────────────────────────────
log "Setting up ~/.config/opencode/ symlinks..."
mkdir -p "$HOME/.config/opencode/skills"

link() {
  local src="$1" dst="$2"
  if [[ -e "$dst" && ! -L "$dst" ]]; then
    warn "$dst exists and is not a symlink — backing up to ${dst}.bak"
    mv "$dst" "${dst}.bak"
  fi
  ln -sf "$src" "$dst"
}

link "$EMACS_DIR/opencode/opencode.json" "$HOME/.config/opencode/opencode.json"
link "$EMACS_DIR/opencode/AGENTS.md"     "$HOME/.config/opencode/AGENTS.md"

for f in "$EMACS_DIR/opencode/skills/"*.md; do
  link "$f" "$HOME/.config/opencode/skills/$(basename "$f")"
done

log "  → ~/.config/opencode/ is now symlinked to $EMACS_DIR/opencode/"

# ── 4. opencode binary ──────────────────────────────────────────────────────
if [[ "$NO_OPENCODE" -eq 0 ]]; then
  if command -v opencode &>/dev/null || [[ -x "$OPENCODE_BIN" ]]; then
    log "opencode already installed: $(command -v opencode || echo "$OPENCODE_BIN")"
  else
    log "Installing opencode..."
    curl -fsSL https://opencode.ai/install | bash
    log "  → opencode installed to ~/.opencode/bin/"
    log "  → Restart your shell or run: export PATH=\"\$HOME/.opencode/bin:\$PATH\""
  fi
fi

# ── 5. Emacs packages + compile ─────────────────────────────────────────────
log "Installing Emacs packages and compiling configuration..."
cd "$EMACS_DIR"
make all

log ""
log "Done! Start Emacs and run M-x opencode."
log ""
log "Next steps:"
log "  1. Fill in credentials: \$EDITOR ~/.emacs.d/.env"
log "  2. Start Emacs: emacs"
log "  3. Launch opencode: M-x opencode"
