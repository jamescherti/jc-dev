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

  SCRIPT_DIR=$(dirname "$(readlink -e "${BASH_SOURCE[0]}")")
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
    git -C "$dir" clean -fxd
    git -C "$dir" reset --hard HEAD
    git -C "$dir" pull --ff-only
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

sync_home() {
  echo "---------------------------------------------------------------------"
  echo " UPDATE-HOME: $HOME"
  echo "---------------------------------------------------------------------"

  local rsync_opts=(--archive
    --exclude '*.elc' --exclude '*.eln'
    --exclude='flymake_*' --exclude='flycheck_*')

  # rsync without delete
  echo "[RUN-RSYNC]" home/ '->' "$HOME/"
  rsync "${rsync_opts[@]}" "$SCRIPT_DIR/home/" "$HOME/"
}

main() {
  init

  if [[ "${JC_DEV_UNATTENDED:-}" = "" ]] \
    && [[ "${JC_DEV_UNATTENDED:-}" -eq 0 ]]; then
    echo
    read -r -p "Install/update? [y,n] " ANSWER
    if [[ "$ANSWER" != "y" ]]; then
      echo "Interrupted." >&2
      exit 1
    fi
  fi

  sync_home

  # JC-DOTFILES
  git_clone \
    https://github.com/jamescherti/jc-dotfiles \
    "$GIT_CLONE_DIR/jc-dotfiles"
  cd "$GIT_CLONE_DIR/jc-dotfiles"
  JC_DOTFILES_UNATTENDED=1 ./install.sh

  # BASH-STDOPS
  git_clone \
    https://github.com/jamescherti/bash-stdops \
    "$GIT_CLONE_DIR/bash-stdops"
  cd "$GIT_CLONE_DIR/bash-stdops"
  PREFIX="$HOME/.local" ./install.sh

  # LIGHTVIM
  git_clone \
    https://github.com/jamescherti/lightvim \
    "$GIT_CLONE_DIR/lightvim"
  mkdir -p ~/.config/nvim
  rm -f ~/.vimrc
  rm -f ~/.config/nvim/init.vim
  cp "$GIT_CLONE_DIR/lightvim/lightvim.vim" ~/.vimrc
  cp "$GIT_CLONE_DIR/lightvim/lightvim.vim" ~/.config/nvim/init.vim

  # JC-FIREFOX-SETTINGS
  LIST_FIREFOX_DIRS=("$HOME/.mozilla/firefox"
    "$HOME/.var/app/org.mozilla.firefox/.mozilla/firefox")
  for firefox_dir in "${LIST_FIREFOX_DIRS[@]}"; do
    if [[ -d "$firefox_dir" ]]; then
      git_clone https://github.com/jamescherti/jc-firefox-settings \
        "$GIT_CLONE_DIR/jc-firefox-settings"
      cd "$GIT_CLONE_DIR/jc-firefox-settings"
      ./install.sh
      break
    fi
  done

  # JC-GNOME-SETTINGS
  if [[ $XDG_CURRENT_DESKTOP = GNOME ]]; then
    # JC-GNOME-SETTINGS
    git_clone \
      https://github.com/jamescherti/jc-gnome-settings \
      "$GIT_CLONE_DIR/jc-gnome-settings"
    cd "$GIT_CLONE_DIR/jc-gnome-settings"
    ./jc-gnome-settings.sh

    # LOCAL GNOME SETTINGS
    cd "$SCRIPT_DIR/data/settings/settings-gnome/"
    ./settings-gnome-keyboard-shortcuts.sh
    ./settings-gnome.sh
  fi

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

  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    cd "$SCRIPT_DIR"
    pwd
    "./data/settings/update-mimetypes.py"
  fi

  # JC-DOTFILES
  git_clone \
    https://github.com/jamescherti/lightemacs \
    "$GIT_CLONE_DIR/lightemacs"
  cd "$GIT_CLONE_DIR/dotfiles"
  git checkout develop

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

  if [[ "${#MY_PIP_PACKAGES[@]}" -gt 1 ]]; then
    if [[ "${VIRTUAL_ENV:-}" ]]; then
      pip install --upgrade pip
      pip install "${MY_PIP_PACKAGES[@]}"
    else
      pip install --user "${MY_PIP_PACKAGES[@]}"
    fi
  fi

  # Fixes the issue where the terminal session is corrupt because of gpg agent
  gpgconf --kill gpg-agent || :
  # pkill -x gpgconf || :
  # pkill -x gpg-agent || :

  echo
  echo Success.
}

main "$@"
