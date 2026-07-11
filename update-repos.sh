#!/usr/bin/env bash

MAX_WORKERS=6

# Go to script dir
SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")
# shellcheck disable=SC2164
cd "$SCRIPT_DIR"

# Variables
SRC_DIR="$HOME/src"
GIT_PULL_MY_REPO="$PWD/.bin/git-pull-my-repo"

if ! [[ -f $GIT_PULL_MY_REPO ]]; then
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
    --exclude ~/src/forks \
    --exclude ~/src/local \
    --parallel \
    --if-exec git-is-clean \
    -- \
    "$BASE_DIR/jc-dev/.bin/$GIT_PULL_MY_REPO"

  git-rexec -j "$MAX_WORKERS" -C "$SRC_DIR/emacs" \
    --exclude ~/src/forks \
    --exclude ~/src/local \
    --parallel \
    --if-exec git-is-clean \
    -- \
    sh -c "git checkout develop && git pull --rebase && git push"
fi
