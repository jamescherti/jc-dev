#!/usr/bin/env bash

set -euf -o pipefail

# shellcheck disable=SC2317
error_handler() {
  local errno="\$?"
  echo "Error: \${BASH_SOURCE[1]}:\${BASH_LINENO[0]}" \
    "(\${BASH_COMMAND} exited with status \$errno)" >&2
  exit "\${errno}"
}

main() {
  trap "error_handler" ERR
  set -o errtrace
  ${1:}
}

main "\$@"
