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
# of this software and associated documentation files (the “Software”), to deal
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

update-repos() {
  # Go to script dir
  cd "$SCRIPT_DIR"

  # Variables
  SRC_DIR="$HOME/src"
  GIT_PULL_MY_REPO="$SCRIPT_DIR/.bin/git-pull-my-repo"

  if ! [[ -f "$GIT_PULL_MY_REPO" ]]; then
    echo "Error: The script doesn't exist: $GIT_PULL_MY_REPO" >&2
    exit 1
  fi

  if [[ -d "$SRC_DIR" ]]; then
    # other than emacs projects
    # git-rexec -C "$SRC_DIR" \
    # --exclude ~/src/forks \
    # --exclude ~/src/local \
    #   --if-exec git-is-clean \
    #   --parallel 'sh -c "git checkout-default && git pull --ff-only"'

    # cd "$SRC_DIR"
    # batchfetch

    echo "[INFO] git pull repos"
    git-rexec -j "$MAX_WORKERS" -C "$SRC_DIR" \
      --exclude "$HOME/src/forks" \
      --exclude "$HOME/src/local" \
      --parallel \
      --if-exec git-is-clean \
      -- \
      "$GIT_PULL_MY_REPO"

    git-rexec -j "$MAX_WORKERS" -C "$SRC_DIR/emacs" \
      --exclude "$HOME/src/forks" \
      --exclude "$HOME/src/local" \
      --parallel \
      --if-exec git-is-clean \
      -- \
      sh -c "git checkout develop && git pull --rebase && git push"
  fi
}

main() {
  update-repos

  cd ~/src
  batchfetch

  git rexec --parallel -C ~/src/ \
    --exclude-dir ~/src/local/ \
    --exclude-dir ~/src/forks/ \
    -- git check-gpg
}

main "$@"
