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

source .lib.inc.sh

MAX_WORKERS=6
SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")

config-firefox() {
  ~/src/dotfiles/jc-firefox-settings/install.sh
}

config-xfce() {
  # JC-XFCE-SETTINGS
  if [[ "${XDG_CURRENT_DESKTOP:-}" = XFCE ]]; then
    cd ~/src/dotfiles/jc-xfce-settings
    ./jc-xfce-settings.sh

    # Local XFCE settings
    "$SCRIPT_DIR/data/settings/settings-xfce4/settings-xfce4.sh"
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

install_python_deps() {
  cd "$SCRIPT_DIR/misc/deps"
  if [[ "${VIRTUAL_ENV:-}" = "" ]]; then
    ./deps-python-pip-user.sh
  else
    ./deps-python-pyenv.sh
  fi
}

UPGRADE_PIP_PACKAGES=0
enable-upgrade-pip-packages() {
  # shellcheck disable=SC2317
  UPGRADE_PIP_PACKAGES=1
}

config-fonts() {
  local fonts_dir=(
    "$HOME/.local/share/fonts"
    "/usr/local/share/fonts"
  )

  local microsoft_fonts_found=0
  local fc_cache_update=0
  local source_conf
  local conf_file
  local conf_dir="${HOME}/.config/fontconfig/conf.d"

  local local_ms_fonts_conf_file="${conf_dir}/50-ms-fonts.conf"

  mkdir -p "${conf_dir}"

  # Check if Microsoft fonts exist in the specified directories
  if ! [[ -f "$local_ms_fonts_conf_file" ]]; then
    local dir
    local match
    for dir in "${fonts_dir[@]}"; do
      if [ -d "$dir" ]; then
        # Look for arial.ttf (case-insensitive) to confirm presence
        match=$(find "$dir" -maxdepth 3 -iname "arial.ttf" -print -quit)
        if [ -n "$match" ]; then
          echo "[FONTS] Microsoft fonts detected in: $dir"
          microsoft_fonts_found=1
          break
        fi
      fi
    done
  else
    microsoft_fonts_found=1
  fi

  if [ "$microsoft_fonts_found" -ne 0 ]; then
    source_conf="data/settings/fontconfig/50-ms-fonts.conf"
    conf_file="$local_ms_fonts_conf_file"
    if [[ -f "$source_conf" ]] \
      && { [[ ! -f "$conf_file" ]] \
        || [[ "$source_conf" -nt "$conf_file" ]]; }; then
      if cp "$source_conf" "${conf_file}"; then
        echo "[FONTS] Fontconfig file created or updated at: ${conf_file}"
        fc_cache_update=1
      fi
    fi
  fi

  source_conf="data/settings/fontconfig/40-custom.conf"
  conf_file="${conf_dir}/40-custom.conf"
  if [[ -f "$source_conf" ]] \
    && { [[ ! -f "$conf_file" ]] \
      || [[ "$source_conf" -nt "$conf_file" ]]; }; then
    if cp "$source_conf" "${conf_file}"; then
      echo "[FONTS] Fontconfig file created or updated at: ${conf_file}"
      fc_cache_update=1
    fi
  fi

  # Update the font cache to apply the changes immediately
  if type -P fc-cache &>/dev/null; then
    if [[ $fc_cache_update -ne 0 ]]; then
      echo "[FONTS] Refreshing font cache..."
      fc-cache -f -v
    else
      echo "[FONTS] There is no need to update fc-cache"
    fi
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

install_gnome_extension() {
  local name="$1"
  local uuid="$2"
  local src_path="$3"
  # local pre_cmd="${4:-}"
  # local post_cmd="${5:-}"
  local dest_dir="${HOME}/.local/share/gnome-shell/extensions/${uuid}/"

  if [[ -z "${name}" || -z "${uuid}" || -z "${src_path}" ]]; then
    echo "[ERROR] Usage: install_gnome_extension <name> <uuid> <source_path>" \
      "[pre_command] [post_command]"
    return 1
  fi

  echo "[INSTALL] ${name}"

  # if [[ -n "${pre_cmd}" ]]; then
  #   eval "${pre_cmd}"
  # fi

  if [[ ! -d "${src_path}" ]]; then
    echo "[ERROR] Source path ${src_path} is invalid or not a directory."
    return 1
  fi

  rm -rf "${dest_dir}"
  mkdir -p "${dest_dir}"

  local rsync_src="${src_path%/}/"
  rsync -ar --delete --delete-excluded --exclude '.git' \
    "${rsync_src}" "${dest_dir}"

  # if [[ -n "${post_cmd}" ]]; then
  #   eval "${post_cmd}"
  # fi

  if gnome-extensions list | grep -q "^${uuid}$"; then
    gnome-extensions enable "${uuid}"
    echo "[INFO] Enabled ${uuid}"
  else
    echo "[WARNING] ${uuid} is not recognized by GNOME Shell. Restart GNOME."
  fi
}

config-gnome() {
  # JC-GNOME-SETTINGS
  if [[ "${XDG_CURRENT_DESKTOP:-}" = GNOME ]]; then
    ~/src/dotfiles/jc-gnome-settings/jc-gnome-settings.sh
    ~/src/dotfiles/jc-gnome-settings/misc/jc-gnome-shortcuts.sh

    # LOCAL GNOME SETTINGS
    local gnome_scripts_path="$SCRIPT_DIR/data/settings/settings-gnome"
    "$gnome_scripts_path/settings-gnome-keyboard-shortcuts.sh"
    "$gnome_scripts_path/settings-gnome.sh"

    # run-or-raise
    install_gnome_extension \
      "run-or-raise" \
      "run-or-raise@edvard.cz" \
      "${HOME}/src/forks/run-or-raise"
    cd ~/.local/share/gnome-shell/extensions/run-or-raise@edvard.cz \
      && glib-compile-schemas schemas

    # HideActivities
    install_gnome_extension \
      "HideActivities" \
      "Hide_Activities@shay.shayel.org" \
      "${HOME}/src/forks/HideActivities"

    # Caffeine
    install_gnome_extension \
      "Caffeine" \
      "caffeine@patapon.info" \
      "${HOME}/src/forks/gnome-shell-extension-caffeine/caffeine@patapon.info"
  fi
}

main() {
  install_python_deps

  config-firefox
  config-gnome
  config-xfce

  config-startup-apps

  config-fonts

  # TODO merge with install_python_deps?
  config-pip-packages

  # Vim packages
  (
    cd ~/.vim_bundle/packpath/pack/git-plugins/start
    batchfetch
  )
}

main "$@"
