#!/usr/bin/env bash
# semacs — fresh install script
# Usage: bash install.sh [--no-opencode] [--no-system-deps]
set -euo pipefail

EMACS_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
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
    sudo apt-get install -y libvterm-dev cmake libenchant-2-dev libtool
  elif command -v pacman &>/dev/null; then
    log "Installing system deps (libvterm, cmake, enchant)..."
    sudo pacman -S --noconfirm libvterm cmake enchant libtool
  elif command -v brew &>/dev/null; then
    log "Installing system deps (libvterm, cmake, enchant)..."
    brew install libvterm cmake enchant libtool
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
make -C "$EMACS_DIR" setup-opencode

# ── 4. opencode binary ──────────────────────────────────────────────────────
if [[ "$NO_OPENCODE" -eq 0 ]]; then
  OPENCODE_BIN="$HOME/.opencode/bin/opencode"
  OPENCODE_CMD="$(command -v opencode 2>/dev/null || echo "")"
  if [[ -n "$OPENCODE_CMD" || -x "$OPENCODE_BIN" ]]; then
    log "opencode already installed: ${OPENCODE_CMD:-$OPENCODE_BIN}"
  else
    log "Installing opencode..."
    curl -fsSL https://opencode.ai/install | bash
    log "  → opencode installed to ~/.opencode/bin/"
    log "  → Restart your shell or run: export PATH=\"\$HOME/.opencode/bin:\$PATH\""
  fi
fi

# ── 5. Emacs packages + compile ─────────────────────────────────────────────
log "Installing Emacs packages and compiling configuration..."
make -C "$EMACS_DIR" all

log ""
log "Done! Start Emacs and run M-x opencode."
log ""
log "Next steps:"
log "  1. Fill in credentials: \$EDITOR ~/.emacs.d/.env"
log "  2. Start Emacs: emacs"
log "  3. Launch opencode: M-x opencode"
