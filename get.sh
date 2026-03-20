#!/usr/bin/env bash
# semacs installer — run with:
#   curl -fsSL https://raw.githubusercontent.com/sevapru/semacs/main/get.sh | bash
set -euo pipefail

REPO="https://github.com/sevapru/semacs.git"
DEST="$HOME/.emacs.d"

log()  { echo "[semacs] $*"; }
warn() { echo "[semacs] WARNING: $*" >&2; }
die()  { echo "[semacs] ERROR: $*" >&2; exit 1; }

command -v git  &>/dev/null || die "git is required"
command -v emacs &>/dev/null || die "emacs is required"

if [[ -d "$DEST/.git" ]]; then
  log "Existing repo found at $DEST — pulling latest..."
  git -C "$DEST" pull --ff-only
else
  if [[ -d "$DEST" ]]; then
    warn "$DEST exists but is not a git repo — backing up to ${DEST}.bak"
    mv "$DEST" "${DEST}.bak"
  fi
  log "Cloning semacs into $DEST..."
  git clone "$REPO" "$DEST"
fi

log "Running install..."
bash "$DEST/install.sh" "$@"
