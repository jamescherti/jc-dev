#!/usr/bin/env bash
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Distributed under terms of the MIT license.
#
# Copyright (C) 2004-2026 James Cherti
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

set -euo pipefail

MAX_WORKERS=6
SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")

SRC_DIR="$HOME/src"
GIT_PULL_MY_REPO="$SCRIPT_DIR/.bin/git-pull-my-repo"

GIT_REXEC_EXCLUDES=(
  "--exclude" "$HOME/src/forks"
  "--exclude" "$HOME/src/local"
)

GIT_REXEC_OPTS=(
  "--exclude" "$HOME/src/forks"
  "--exclude" "$HOME/src/local"
  "--if-exec" "git-is-clean"
  "--parallel")

# shellcheck disable=SC2317
error_handler() {
  local errno="$?"
  echo "Error: ${BASH_SOURCE[1]}:${BASH_LINENO[0]}" \
    "(${BASH_COMMAND} exited with status $errno)" >&2
  exit "${errno}"
}

trap "error_handler" ERR
set -o errtrace

update-repos() {
  # Go to script dir
  # cd "$SCRIPT_DIR"

  if [[ -d "$SRC_DIR" ]]; then
    return
  fi

  if ! [[ -f "$GIT_PULL_MY_REPO" ]]; then
    echo "Error: The script doesn't exist: $GIT_PULL_MY_REPO" >&2
    exit 1
  fi

  # TODO
  # cd "$SRC_DIR"
  # batchfetch

  echo "[INFO] git pull repositories"

  git-rexec -C "$SRC_DIR" \
    "${GIT_REXEC_OPTS[@]}" -- \
    sh -c "git checkout-default && git pull --ff-only && '$GIT_PULL_MY_REPO'"

  # TODO pass multiple paths
  git-rexec -j "$MAX_WORKERS" -C "$SRC_DIR/emacs" \
    "${GIT_REXEC_OPTS[@]}" -- \
    sh -c "git checkout develop && git pull --rebase && git push"

  git-rexec -j "$MAX_WORKERS" -C "$SRC_DIR/python" \
    "${GIT_REXEC_OPTS[@]}" -- \
    sh -c "git checkout develop && git pull --rebase && git push"
}

config-project-list() {
  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    "$SCRIPT_DIR/home/.bin/update-project-list"
  fi
}

install-pip-packages() {
  local pip_opts=()

  if ! [[ "${VIRTUAL_ENV:-}" ]]; then
    pip_opts+=(--user)
  fi

  pip install "${pip_opts[@]}" --no-build-isolation --no-deps \
    "$MY_PYTHON_PACKAGES_PATH/git-rexec" \
    "$MY_PYTHON_PACKAGES_PATH/batchfetch"
}

main() {
  cd "$SRC_DIR/dotfiles/jc-dev"
  git pull

  install-pip-packages

  # This has to be launched first because it ensures all missing repositories
  # are present
  cd "$SRC_DIR"
  rsync -a \
    "$SCRIPT_DIR/home/src/batchfetch.yaml" \
    "$SRC_DIR/batchfetch.yaml"

  update-repos

  config-project-list

  git rexec --parallel -C $SRC_DIR/ \
    "${GIT_REXEC_EXCLUDES[@]}" -- \
    git-check-gpg
}

main "$@"
