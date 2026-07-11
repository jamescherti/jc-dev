#!/usr/bin/env bash

# shellcheck disable=SC2317
error_handler() {
  local errno="$?"
  echo "Error: ${BASH_SOURCE[1]}:${BASH_LINENO[0]}" \
    "(${BASH_COMMAND} exited with status $errno)" >&2
  exit "${errno}"
}

main() {
  trap "error_handler" ERR
  set -o errtrace

  # Run or raise
  echo "[INSTALL] run-or-raise"
  cd run-or-raise/
  make compile >/dev/null
  make build >/dev/null
  rm -fr ~/.local/share/gnome-shell/extensions/run-or-raise@edvard.cz
  mkdir -p ~/.local/share/gnome-shell/extensions/run-or-raise@edvard.cz
  unzip "build/run-or-raise@edvard.cz.shell-extension.zip" -d ~/.local/share/gnome-shell/extensions/run-or-raise@edvard.cz
  cp schemas/gschemas.compiled ~/.local/share/gnome-shell/extensions/run-or-raise\@edvard.cz/schemas/
}

main "$@"
