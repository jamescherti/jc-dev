#!/usr/bin/env bash
#
# Describe: Install the jc-dev development environment
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

# shellcheck disable=SC2269
set -euf -o pipefail

GIT_CLONE_DIR="$HOME/.jc-dev"

# shellcheck disable=SC2317
error_handler() {
  local errno="$?"
  echo "Error: ${BASH_SOURCE[1]}:${BASH_LINENO[0]}" \
    "(${BASH_COMMAND} exited with status $errno)" >&2
  exit "${errno}"
}

init() {
  trap "error_handler" ERR
  set -o errtrace

  if [ "$(id -u)" -eq "0" ]; then
    echo "Error: you cannot run this script as root." >&2
    exit 1
  fi

  local cmd
  for cmd in rsync pip python git readlink; do
    if ! type -P "$cmd" &>/dev/null; then
      echo "Error: the '$cmd' command was not found." >&2
      exit 1
    fi
  done

  SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")
  mkdir -p "$GIT_CLONE_DIR"

  # Check if terminal supports color
  if [ -t 1 ] && [ "$(tput colors)" -ge 8 ]; then
    # \e[1;33m is Bold Yellow / Light Yellow
    YELLOW_COLOR=$(
      tput setaf 3
      tput bold
    )
    RESET_COLOR=$(tput sgr0)
  else
    YELLOW_COLOR=""
    RESET_COLOR=""
  fi
}

git_clone() {
  local url="$1"
  local dir="$2"

  echo

  echo "${YELLOW_COLOR}[STEP] $url${RESET_COLOR}"
  dir=$(readlink -m "$dir")

  if ! [[ -d "$dir" ]]; then
    local parent
    parent=$(dirname "$dir")
    mkdir -p "$parent"

    git clone -q "$url" "$dir"
  else
    git -C "$dir" fetch --all --prune
    git -C "$dir" clean -fxd
    git -C "$dir" reset --hard '@{upstream}'
  fi
}

secure_dir() {
  if [[ -d "$1" ]]; then
    echo "[Secure Dir] $1"
    chmod 700 "$1"
  elif [[ -f "$1" ]]; then
    echo "[Secure File] $1"
    chmod 600 "$1"
  else
    echo "[Ignore Securing] $1"
  fi
}

copy_dotfiles() {
  echo "---------------------------------------------------------------------"
  echo " UPDATE-HOME: $HOME"
  echo "---------------------------------------------------------------------"

  local rsync_opts=(--archive
    --exclude '*.elc' --exclude '*.eln'
    --exclude '*.git'
    --exclude='flymake_*' --exclude='flycheck_*')

  # rsync without delete
  echo "[RUN-RSYNC]" home/ '->' "$HOME/"
  rsync "${rsync_opts[@]}" "$SCRIPT_DIR/home/" "$HOME/"
}

confirm() {
  if [[ "${JC_DEV_UNATTENDED:-}" = "" ]] \
    && [[ "${JC_DEV_UNATTENDED:-}" -eq 0 ]]; then
    echo
    read -r -p "Install/update? [y,n] " ANSWER
    if [[ "$ANSWER" != "y" ]]; then
      echo "Interrupted." >&2
      exit 1
    fi
  fi
}

config-jc-dotfiles() {
  # JC-DOTFILES
  git_clone \
    https://github.com/jamescherti/jc-dotfiles \
    "$GIT_CLONE_DIR/jc-dotfiles"
  cd
  JC_DOTFILES_UNATTENDED=1 "$GIT_CLONE_DIR/jc-dotfiles/install.sh"
}

config-lightvim() {
  # LIGHTVIM
  git_clone \
    https://github.com/jamescherti/lightvim \
    "$GIT_CLONE_DIR/lightvim"
  mkdir -p ~/.config/nvim
  rm -f ~/.vimrc
  rm -f ~/.config/nvim/init.vim
  cp "$GIT_CLONE_DIR/lightvim/lightvim.vim" ~/.vimrc
  cp "$GIT_CLONE_DIR/lightvim/lightvim.vim" ~/.config/nvim/init.vim
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
  if [[ $XDG_CURRENT_DESKTOP = GNOME ]]; then
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
  if [[ $XDG_CURRENT_DESKTOP = XFCE ]]; then
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

config-bash-stdops() {
  # BASH-STDOPS
  git_clone \
    https://github.com/jamescherti/bash-stdops \
    "$GIT_CLONE_DIR/bash-stdops"
  cd "$GIT_CLONE_DIR/bash-stdops"
  PREFIX="$HOME/.local" ./install.sh
}

config-mimetypes() {
  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    "$SCRIPT_DIR/data/settings/update-mimetypes.py"
  fi
}

config-project-list() {
  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    "$SCRIPT_DIR/home/.bin/update-project-list"
  fi
}

config-startup-apps() {
  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    mkdir -p ~/.config/autostart
    {
      echo "[Desktop Entry]"
      echo "Name=x-startup-apps"
      echo "Comment=x-startup-apps"
      echo "Keywords=x-startup-apps"
      echo "Exec=$HOME/.bin/xdevenv startup-apps"
      echo "Icon=org.gnome.ArchiveManager"
      echo "Type=Application"
      echo "StartupNotify=false"
    } >"$HOME/.config/autostart/x-startup-apps.desktop"
  fi
}

config-lightemacs() {
  if [[ -f ~/src/emacs/lightemacs/ ]]; then
    if git-is-clean ~/src/emacs/lightemacs/; then
      git -C ~/src/emacs/lightemacs/ checkout develop
      git -C ~/src/emacs/lightemacs/ pull --rebase
    fi
  else
    git_clone \
      https://github.com/jamescherti/lightemacs \
      "$GIT_CLONE_DIR/lightemacs"
    git -C "$GIT_CLONE_DIR/lightemacs" checkout develop
  fi
}

config-files() {
  # Secure dirs
  secure_dir ~/.gnupg
  secure_dir ~/.ssh
  secure_dir ~/Documents
  secure_dir ~/Downloads
  secure_dir ~/src
  secure_dir ~/Sync
  secure_dir ~/.emacs-data
  secure_dir ~/.vim
  secure_dir ~/.vim_bundle
  secure_dir ~/.bash_history
  secure_dir ~/

  if ! [[ -f ~/.sync-spell-dict ]]; then
    touch ~/.sync-spell-dict
  fi

  cp "$SCRIPT_DIR/.default_pathaction.yaml" "$HOME/.pathaction.yaml"

  mkdir -p ~/.git-templates
  if ! [[ -L ~/.git-templates/hooks ]]; then
    # rm -f ~/.git-templates/hooks
    ln -sf ~/.git-hooks/ ~/.git-templates/hooks
  fi
}

enable-upgrade-pip-packages() {
  UPGRADE_PIP_PACKAGES=1
}

config-pip-packages() {
  UPGRADE_PIP_PACKAGES=0
  run_every 172800 \
    ~/.cache/jc-dev.pip-upgrade \
    enable-upgrade-pip-packages

  # PIP
  MY_PIP_PACKAGES=()
  if ! type -P pathaction &>/dev/null; then
    MY_PIP_PACKAGES+=(pathaction)
  fi

  if ! type -P ultyas &>/dev/null; then
    MY_PIP_PACKAGES+=(ultyas)
  fi

  if ! type -P git-commitflow &>/dev/null; then
    MY_PIP_PACKAGES+=(git-commitflow)
  fi

  if ! type -P batchfetch &>/dev/null; then
    MY_PIP_PACKAGES+=(batchfetch)
  fi

  if ! type -P git-smartmv &>/dev/null; then
    MY_PIP_PACKAGES+=(git-smartmv)
  fi

  local opts=()

  if [[ $UPGRADE_PIP_PACKAGES -ne 0 ]]; then
    opts+=(--upgrade)
  fi

  if [[ "${#MY_PIP_PACKAGES[@]}" -gt 1 ]]; then
    if [[ "${VIRTUAL_ENV:-}" ]]; then
      pip install --upgrade pip
      pip install "${opts[@]}" "${MY_PIP_PACKAGES[@]}"
    else
      pip install "${opts[@]}" --user "${MY_PIP_PACKAGES[@]}"
    fi
  fi

}

run_every() {
  # Ensure we have at least 3 arguments
  if [[ $# -lt 3 ]]; then
    echo "Usage: run_every <seconds> <reference_file> <command...>" >&2
    return 1
  fi

  local interval="$1"
  local lock_file="$2"
  shift 2
  local command=("$@")

  # Ensure the directory exists
  mkdir -p "$(dirname "$lock_file")"

  if [[ -f "$lock_file" ]]; then
    local current_time
    current_time=$(date +%s)
    local last_run
    last_run=$(stat -c %Y "$lock_file")
    local elapsed
    elapsed=$((current_time - last_run))

    if [[ $elapsed -ge $interval ]]; then
      "${command[@]}"
      touch "$lock_file"
    else
      echo "[IGNORED] $*"
    fi
  else
    # First time running: execute and create the file
    "${command[@]}"
    touch "$lock_file"
  fi
}

git_maintenance() {
  # The git fsck --full command is used to verify the integrity of a Git
  # repository. Here’s a detailed breakdown of what it does:
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

  if [[ -d "$HOME/src" ]]; then
    # shellcheck disable=SC2016
    "$SCRIPT_DIR/home/.bin/git-find-repos" \
      "$HOME/src" \
      --if-exec git-is-clean \
      --exec-bg git-maintenance
  fi
}

main() {
  init
  confirm

  copy_dotfiles
  config-jc-dotfiles
  config-bash-stdops
  config-lightvim

  config-project-list
  config-startup-apps

  config-lightemacs
  config-files

  config-pip-packages

  config-firefox
  config-gnome
  config-xfce
  config-mimetypes

  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    "$SCRIPT_DIR/home/.bin/update-emacs-config"
  fi

  # 1 day = 86400
  run_every 86400 \
    ~/.cache/jc-dev.git_maintenance \
    "$SCRIPT_DIR/home/.bin/git_maintenance"

  echo
  echo Success.
}

main "$@"
