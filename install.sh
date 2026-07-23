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
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

# shellcheck disable=SC2269
set -euf -o pipefail

# TODO remove
# GIT_CLONE_DIR="$HOME/.jc-dev"

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

  export PATH="$HOME/.local/bin:$HOME/.bin:$PATH"

  local cmd
  for cmd in rsync pip python git readlink; do
    if ! type -P "$cmd" &>/dev/null; then
      echo "Error: the '$cmd' command was not found." >&2
      exit 1
    fi
  done

  SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")
  # mkdir -p "$GIT_CLONE_DIR"

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

    local branch
    branch=$(git -C "$dir" symbolic-ref --short HEAD)

    # Reset explicitly to the matching branch on origin
    git -C "$dir" reset --hard "origin/$branch"
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
  # git_clone \
  #   https://github.com/jamescherti/jc-dotfiles \
  #   "$GIT_CLONE_DIR/jc-dotfiles"
  # cd
  # JC_DOTFILES_UNATTENDED=1 "$GIT_CLONE_DIR/jc-dotfiles/install.sh"
  JC_DOTFILES_UNATTENDED=1 "$HOME/src/dotfiles/jc-dotfiles//install.sh"
}

config-lightvim() {
  cd ~/src/dotfiles/lightvim
  # LIGHTVIM
  # git_clone \
  #   https://github.com/jamescherti/lightvim \
  #   "$GIT_CLONE_DIR/lightvim"
  mkdir -p ~/.config/nvim
  rm -f ~/.vimrc
  rm -f ~/.config/nvim/init.vim
  cp "$HOME/src/dotfiles/lightvim/lightvim.vim" ~/.vimrc
  cp "$HOME/src/dotfiles/lightvim/lightvim.vim" ~/.config/nvim/init.vim
  # cp "$GIT_CLONE_DIR/lightvim/lightvim.vim" ~/.vimrc
  # cp "$GIT_CLONE_DIR/lightvim/lightvim.vim" ~/.config/nvim/init.vim
}

config-bash-stdops() {
  # BASH-STDOPS
  # git_clone \
  #   https://github.com/jamescherti/bash-stdops \
  #   "$GIT_CLONE_DIR/bash-stdops"
  # cd "$GIT_CLONE_DIR/bash-stdops"
  cd ~/src/dotfiles/bash-stdops/
  PREFIX="$HOME/.local" ./install.sh
}

config-project-list() {
  if [[ "${XDG_CURRENT_DESKTOP:-}" != "" ]]; then
    "$SCRIPT_DIR/home/.bin/update-project-list"
  fi
}

# config-lightemacs() {
#   if [[ -d ~/src/emacs/lightemacs/ ]]; then
#     if git-is-clean ~/src/emacs/lightemacs/; then
#       git -C ~/src/emacs/lightemacs/ checkout develop
#       git -C ~/src/emacs/lightemacs/ pull --rebase
#     fi
#   else
#     git_clone \
#       https://github.com/jamescherti/lightemacs \
#       "$GIT_CLONE_DIR/lightemacs"
#     git -C "$GIT_CLONE_DIR/lightemacs" checkout develop
#   fi
# }

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

  cp "$SCRIPT_DIR/.default_pathaction.yaml" "$HOME/.pathaction.yaml"

  mkdir -p ~/.git-templates
  if ! [[ -L ~/.git-templates/hooks ]]; then
    # rm -f ~/.git-templates/hooks
    ln -sf ~/.git-hooks/ ~/.git-templates/hooks
  fi
}

config_gpg() {
  chmod 700 "$HOME/.gnupg/"
  mkdir -p "$HOME/.ssh"
  chmod 700 "$HOME/.ssh/"

  # ln -sf ~/.git
  cp "$SCRIPT_DIR/.gpg-agent.conf" ~/.gnupg/gpg-agent.conf

  local pinentry_bin
  pinentry_bin=$(type -P pinentry-curses)
  if [[ $OSTYPE =~ linux ]] && [[ "$pinentry_bin" != "" ]]; then
    # Linux specific. I added this condition because pinentry-curses does not
    # work on macOS.
    echo "pinentry-program /usr/bin/pinentry-curses" >>~/.gnupg/gpg-agent.conf
  fi
}

main() {
  init
  confirm

  rm -f ~/.gitignore_global

  copy_dotfiles
  config-jc-dotfiles
  config_gpg
  config-bash-stdops

  config-lightvim

  config-project-list

  # config-lightemacs
  config-files

  "$SCRIPT_DIR/home/.bin/update-emacs-config"

  echo
  echo "[INFO] Update spell check dictionary"
  if ! [[ -f ~/.sync-spell-dict ]]; then
    touch ~/.sync-spell-dict
  fi

  sync-spell-dict

  echo
  echo "[INFO] Update Emacs templates"
  update-emacs-templates

  echo "[INFO] Install desktop files"
  rsync -a "$SCRIPT_DIR/data/dist/desktop/" "$HOME/"

  echo
  echo "[INFO] Update Emacs snippets"
  update-emacs-snippets

  # Legacy files. Replace with .ignore files
  # TODO remove this
  rm -f ~/.rgignore ~/.fdignore ~/src/.rgignore ~/src/.fdignore

  echo
  echo Success.
}

main "$@"
