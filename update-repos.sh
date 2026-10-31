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

SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")

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

  # Git checkout to default branch and git pull --ff-only
  local git_pull_my_repo="$SCRIPT_DIR/home/.bin/git-pull-my-repo"

  if ! [[ -f "$git_pull_my_repo" ]]; then
    echo "Error: The script doesn't exist: $git_pull_my_repo" >&2
    exit 1
  fi

  # TODO
  # cd "$SRC_DIR"
  # batchfetch

  echo "[INFO] git pull repositories"

  git-rexec -C "$SRC_DIR" \
    "${GIT_REXEC_OPTS[@]}" -- \
    sh -c "git checkout-default && '$git_pull_my_repo'"

  # TODO pass multiple paths
  git-rexec -j "$GIT_PULL_WORKERS" -C "$SRC_DIR/emacs" \
    "${GIT_REXEC_OPTS[@]}" -- \
    sh -c "git checkout develop && git pull --rebase && git push"

  git-rexec -j "$GIT_PULL_WORKERS" -C "$SRC_DIR/python" \
    "${GIT_REXEC_OPTS[@]}" -- \
    sh -c "git checkout develop && git pull --rebase && git push"

  git-rexec -j "$GIT_PULL_WORKERS" -C "$SRC_DIR/tools" \
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

bf-or-git() {
  if [[ -f "$1" ]]; then
    batchfetch "$1"
  else
    git -C "$1" checkout "$BF_OR_GIT_BRANCH"
    git -C "$1" pull
  fi
}

prepare-dependencies() {
  if ! [[ -d "$SRC_DIR/python/batchfetch/" ]]; then
    git clone \
      "git@github.com:jamescherti/batchfetch" \
      "$SRC_DIR/python/batchfetch/"
  fi

  rsync -a \
    "$SCRIPT_DIR/home/src/batchfetch.yaml" \
    "$SRC_DIR/batchfetch.yaml"

  BF_OR_GIT_BRANCH=main
  bf-or-git "$SRC_DIR/dotfiles/jc-dev"
  BF_OR_GIT_BRANCH=develop
  bf-or-git "$SRC_DIR/python/batchfetch/"
  bf-or-git "$SRC_DIR/python/git-rexec/"

  set +x
}

git_maintenance() {
  # The git fsck --full command is used to verify the integrity of a Git
  # repository. Here's a detailed breakdown of what it does:
  #
  # The git fsck --full command is a comprehensive utility for verifying the
  # integrity of a Git repository. By performing a thorough examination of all
  # objects and references within the repository, it ensures that all data is
  # correctly linked and free from corruption. This command checks the
  # consistency of blobs, trees, commits, and tags, as well as the validity of
  # branch and tag references. The --full option makes the check more
  # exhaustive, identifying any discrepancies or damaged objects and providing
  # detailed reports on potential issues. It is especially useful for diagnosing
  # repository problems after unexpected failures or for routine maintenance to
  # ensure ongoing repository health.
  # git find git fsck --full

  # git gc: Runs garbage collection, which cleans up unnecessary files and
  # optimizes the local repository.
  #
  # --aggressive: This option makes the garbage collection process more
  # --thorough,
  #
  # --resulting in better compression of objects and a more optimized
  # --repository.
  #
  # --However, it can be significantly slower and more resource-intensive than
  # --the

  # --default garbage collection.
  #
  # --prune=now: This option tells Git to remove all objects that are not
  # --reachable from any reference, including those that are less than two weeks
  # --old, which is the default. Using now removes all such objects immediately.
  # git find git gc --aggressive --prune=now

  if [[ -d "$SRC_DIR" ]]; then
    # shellcheck disable=SC2016
    git-rexec \
      -C "$SRC_DIR" \
      --if-exec git-is-clean \
      --parallel \
      -- \
      "$SCRIPT_DIR/home/.bin/git-regular-maintenance"
  fi
}

main() {
  cd "$SCRIPT_DIR"
  # shellcheck disable=SC1091
  source .jc-dev.inc.sh

  # This has to be launched first because it ensures all missing repositories
  # are present
  mkdir -p "$SRC_DIR"
  cd "$SRC_DIR"

  prepare-dependencies

  install-pip-packages

  update-repos

  config-project-list

  git rexec --parallel -C "$SRC_DIR/" \
    "${GIT_REXEC_EXCLUDES[@]}" -- \
    git-check-gpg

  # TODO Run once a day
  git_maintenance
}

main "$@"
