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
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

set -euo pipefail

MAX_WORKERS=6

update-repos() {
  # Go to script dir
  SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")
  # shellcheck disable=SC2164
  cd "$SCRIPT_DIR"

  # Variables
  SRC_DIR="$HOME/src"
  GIT_PULL_MY_REPO="$PWD/.bin/git-pull-my-repo"

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

config-firefox() {
  # JC-FIREFOX-SETTINGS
  LIST_FIREFOX_DIRS=("$HOME/.mozilla/firefox"
    "$HOME/.var/app/org.mozilla.firefox/.mozilla/firefox")
  for firefox_dir in "${LIST_FIREFOX_DIRS[@]}"; do
    if [[ -d "$firefox_dir" ]]; then
      git_clone https://github.com/jamescherti/jc-firefox-settings \
        "$GIT_CLONE_DIR/jc-firefox-settings"
      "$GIT_CLONE_DIR/jc-firefox-settings/install.sh"
      break
    fi
  done
}

config-gnome() {
  # JC-GNOME-SETTINGS
  if [[ "${XDG_CURRENT_DESKTOP:-}" = GNOME ]]; then
    # JC-GNOME-SETTINGS
    git_clone \
      https://github.com/jamescherti/jc-gnome-settings \
      "$GIT_CLONE_DIR/jc-gnome-settings"
    "$GIT_CLONE_DIR/jc-gnome-settings/jc-gnome-settings.sh"

    # LOCAL GNOME SETTINGS
    local gnome_scripts_path="$SCRIPT_DIR/data/settings/settings-gnome"
    "$gnome_scripts_path/settings-gnome-keyboard-shortcuts.sh"
    "$gnome_scripts_path/settings-gnome.sh"
  fi
}

config-xfce() {
  # JC-XFCE-SETTINGS
  if [[ "${XDG_CURRENT_DESKTOP:-}" = XFCE ]]; then
    # JC-XFCE-SETTINGS
    git_clone \
      https://github.com/jamescherti/jc-xfce-settings \
      "$GIT_CLONE_DIR/jc-xfce-settings"
    cd "$GIT_CLONE_DIR/jc-xfce-settings"
    ./jc-xfce-settings.sh

    # Local XFCE settings
    cd "$SCRIPT_DIR/data/settings/settings-xfce4/"
    ./settings-xfce4.sh
  fi
}

install_python_deps() {
  cd "$SCRIPT_DIR/misc/deps"
  if [[ "${VIRTUAL_ENV:-}" = "" ]]; then
    ./deps-python-pip-user.sh
  else
    ./deps-python-pyenv.sh
  fi
}

config-pip-packages() {
  run_every 172800 \
    ~/.cache/jc-dev.pip-upgrade \
    enable-upgrade-pip-packages

  # PIP
  MY_PIP_PACKAGES=()
  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]] \
    || ! type -P pathaction &>/dev/null; then
    MY_PIP_PACKAGES+=(pathaction)
  fi

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]] \
    || ! type -P ultyas &>/dev/null; then
    MY_PIP_PACKAGES+=(ultyas)
  fi

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]] \
    || ! type -P git-rexec &>/dev/null; then
    MY_PIP_PACKAGES+=(git+https://github.com/jamescherti/git-rexec)
  fi

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]] \
    || ! type -P git-commitflow &>/dev/null; then
    MY_PIP_PACKAGES+=(git-commitflow)
  fi

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]] \
    || ! type -P batchfetch &>/dev/null; then
    MY_PIP_PACKAGES+=(batchfetch)
  fi

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]] \
    || ! type -P git-smartmv &>/dev/null; then
    MY_PIP_PACKAGES+=(git-smartmv)
  fi

  local opts=()

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]]; then
    opts+=(--upgrade)
  fi

  if [[ "${#MY_PIP_PACKAGES[@]}" -gt 1 ]]; then
    if [[ "${VIRTUAL_ENV:-}" ]]; then
      printf "[PIP INSTALL] %s\n" "pip install --upgrade pip"
      pip install --upgrade pip

      printf "[PIP INSTALL] %s\n" "pip install ${MY_PIP_PACKAGES[*]}"
      pip install "${opts[@]}" "${MY_PIP_PACKAGES[@]}"
    else
      printf "[PIP INSTALL] %s\n" "pip install --user ${MY_PIP_PACKAGES[*]}"
      pip install "${opts[@]}" --user "${MY_PIP_PACKAGES[@]}"
    fi
  fi
}

config-mimetypes() {
  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    "$SCRIPT_DIR/data/settings/update-mimetypes.py"
  fi
}

main() {
  install_python_deps

  update-repos

  config-firefox
  config-gnome
  config-xfce

  config-mimetypes

  # TODO merge with install_python_deps?
  config-pip-packages
}

main "$@"
