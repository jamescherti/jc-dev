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

# shellcheck disable=SC2269
set -euf -o pipefail

BASE_DIR="$HOME/.jc-dev"
mkdir -p "$BASE_DIR"

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

  # BASH-STDOPS
  git_clone https://github.com/jamescherti/bash-stdops "$BASE_DIR/bash-stdops"
  cd "$BASE_DIR/bash-stdops"
  PREFIX="$HOME/.local" ./install.sh

  # LIGHTVIM
  git_clone https://github.com/jamescherti/lightvim "$BASE_DIR/lightvim"
  mkdir -p ~/.config/nvim
  rm -f ~/.vimrc
  rm -f ~/.config/nvim/init.vim
  cp ~/.home-update/lightvim/lightvim.vim ~/.vimrc
  cp ~/.home-update/lightvim/lightvim.vim ~/.config/nvim/init.vim

  # JC-DOTFILES
  git_clone https://github.com/jamescherti/jc-dotfiles "$BASE_DIR/jc-dotfiles"
  cd "$BASE_DIR/jc-dotfiles"
  JC_DEV_UNATTENDED=1 ./install.sh

  # JC-FIREFOX-SETTINGS
  LIST_FIREFOX_DIRS=("$HOME/.mozilla/firefox"
    "$HOME/.var/app/org.mozilla.firefox/.mozilla/firefox")
  for firefox_dir in "${LIST_FIREFOX_DIRS[@]}"; do
    if [[ -d "$firefox_dir" ]]; then
      git_clone https://github.com/jamescherti/jc-firefox-settings \
        "$BASE_DIR/jc-firefox-settings"
      cd "$BASE_DIR/jc-firefox-settings"
      ./install.sh
      break
    fi
  done

  # JC-GNOME-SETTINGS
  if [[ $XDG_CURRENT_DESKTOP = GNOME ]]; then
    git_clone \
      https://github.com/jamescherti/jc-gnome-settings \
      "$BASE_DIR/jc-gnome-settings"
    ./jc-gnome-settings.sh
  fi

  # JC-XFCE-SETTINGS
  if [[ $XDG_CURRENT_DESKTOP = XFCE ]]; then
    git_clone \
      https://github.com/jamescherti/jc-xfce-settings \
      "$BASE_DIR/jc-xfce-settings"
    ./jc-xfce-settings.sh
  fi
}

main "$@"
