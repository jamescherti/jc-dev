#!/usr/bin/env bash

RUN_EMACS_BIN="${RUN_EMACS_BIN:-}"
OSFAMILY=$(~/.local/bin/osid 2>/dev/null || echo "unknown")

if ! [[ -f "$RUN_EMACS_BIN" ]]; then
  export \
    RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-master/bin/emacs"
  if [[ $EMACS_D = "" ]]; then
    EMACS_D="$HOME/.emacs-master.d/"
  fi
fi

if ! [[ -f "$RUN_EMACS_BIN" ]]; then
  # export \
  #   RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-master/bin/emacs"
  export RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-emacs-31/bin/emacs"
  if [[ $EMACS_D = "" ]]; then
    EMACS_D="$HOME/.emacs-master.d/"
  fi
fi

# if [[ $OSFAMILY = gentoo ]] || [[ $OSFAMILY = arch ]]; then
#   export \
#     RUN_EMACS_BIN="/opt/local/$USER/$OSFAMILY/emacs/branch-master/bin/emacs"
#   if [[ $EMACS_D = "" ]]; then
#     EMACS_D="$HOME/.emacs-master.d/"
#   fi
# fi

#if ! [[ -f "$RUN_EMACS_BIN" ]]; then
#  export RUN_EMACS_BIN="/usr/bin/emacs"
#  if [[ $EMACS_D = "" ]]; then
#    EMACS_D="$HOME/.emacs-use-package.d/"
#  fi
#fi

echo "[INFO] Emacs binary: $RUN_EMACS_BIN"
if ! [[ -f "$RUN_EMACS_BIN" ]]; then
  echo "Error: The Emacs binary does not exist: $RUN_EMACS_BIN" >&2
  exit 1
fi

if ! [[ -d "$EMACS_D" ]]; then
  echo "Error: The Emacs user directory does not exist: '$EMACS_D'" >&2
  exit 1
fi
export EMACSNATIVELOADPATH="$EMACS_D/var/eln-cache/:"
export RUN_EMACS_BIN
