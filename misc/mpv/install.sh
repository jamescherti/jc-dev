#!/usr/bin/env bash
SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")
cd "$SCRIPT_DIR"
rsync -a scripts script-modules ~/.config/mpv/
