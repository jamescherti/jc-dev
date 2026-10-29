#!/usr/bin/env bash

set -x

RUN_EMACS_BIN="${RUN_EMACS_BIN:-}"
OSFAMILY=$(
  # shellcheck disable=SC1091
  [[ -r /etc/os-release ]] && source /etc/os-release 2>/dev/null
  echo "${ID:-unknown}"
)

EMACS_D=""

# RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/tag-emacs-31.1/bin/emacs"
# EMACS_D="$HOME/.emacs-straight.d/"

# # export RUN_EMACS_BIN="/usr/bin/emacs"
# RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-master/bin/emacs"
# EMACS_D="$HOME/.emacs-straight.d/"
#

RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-emacs-31/bin/emacs"
EMACS_D="$HOME/.emacs-straight.d/"

#
# RUN_EMACS_BIN="/usr/bin/emacs"
# EMACS_D="$HOME/.emacs-use-package.d/"
#
# RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-master/bin/emacs"
# EMACS_D="$HOME/.emacs-master.d/"
#
# RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-emacs-31/bin/emacs"
# EMACS_D="$HOME/.emacs-master.d/"

echo "[INFO] Emacs binary: $RUN_EMACS_BIN"
if ! [[ -f "$RUN_EMACS_BIN" ]]; then
  echo "Error: The Emacs binary does not exist: $RUN_EMACS_BIN" >&2
  exit 1
fi

if ! [[ -d "$EMACS_D" ]]; then
  echo "Error: The Emacs user directory does not exist: '$EMACS_D'" >&2
  exit 1
else
  export EMACSNATIVELOADPATH="$EMACS_D/var/eln-cache/:"
fi
