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

set -euf -o pipefail

SHORTCUT_INDEX=0
SHORTCUT_STRING=""

# run() {
#   printf "%s\n" "$*"
#   "$@" || return 1
# }
#
# gset() {
#   gsettings set "$@" || return 1
#   return 0
# }

shortcut() {
  local shortcut_name="'$1'"
  local shortcut="'$2'"
  local shortcut_cmd="'$3'"

  local uri="org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom$SHORTCUT_INDEX/"
  gsettings set "$uri" name "$shortcut_name" >/dev/null
  gsettings set "$uri" binding "$shortcut" >/dev/null
  gsettings set "$uri" command "$shortcut_cmd" >/dev/null

  # printf "Changing shortcut: %s %s %s\n" "$1" "$2" "$3"

  # if [[ $SHORTCUT_STRING != "" ]]; then
  #   SHORTCUT_STRING="${SHORTCUT_STRING},"
  # fi
  # SHORTCUT_STRING="${SHORTCUT_STRING}'/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom${SHORTCUT_INDEX}/'"

  SHORTCUT_INDEX=$((SHORTCUT_INDEX + 1))
}

# shellcheck disable=SC2317
error_handler() {
  local errno="$?"
  echo "Error: ${BASH_SOURCE[1]}:${BASH_LINENO[0]}" \
    "(${BASH_COMMAND} exited with status $errno)" >&2
  exit "${errno}"
}

trap "error_handler" ERR
set -o errtrace

SCRIPT_DIR=$(dirname "$(realpath "${BASH_SOURCE[0]}")")
cd "$SCRIPT_DIR"

echo "Updateing GNOME shortcuts..."

if type -P xdevenv &>/dev/null; then
  shortcut "xocrshot" "<Primary><Alt>O" "$HOME/.bin/xdevenv xocrshot"
  shortcut "Screenshot" "<Primary><Alt>C" "$HOME/.bin/xdevenv screenshot"
  shortcut "Editor" "<Primary><Alt>V" "$HOME/.bin/xdevenv editor"
  shortcut "Terminal" "<Primary><Alt>R" "$HOME/.bin/xdevenv terminal"
  shortcut "Web Browser" "<Primary><Alt>F" "$HOME/.bin/xdevenv web-browser"
  shortcut "Pwd Manager" "<Primary><Alt>S" "$HOME/.bin/xdevenv pwd-manager"
  shortcut "Mail Client" "<Primary><Alt>Q" "$HOME/.bin/xdevenv mail-client"
  shortcut "Suspend" "<Primary>F12" "dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true"
  shortcut "File Explorer" "<Primary><Alt>E" "$HOME/.bin/xdevenv file-explorer"
  shortcut "Firefox Profiles" "<Primary><Alt>P" "$HOME/.bin/xdevenv web-browser-profiles"
  shortcut "Changer user" "<Primary><Alt>U" "$HOME/.bin/xdevenv gdm-change-user"
  shortcut "Light" "<Primary><Alt>M" "$HOME/.bin/xdevenv light"
  shortcut "Night" "<Primary><Alt>N" "$HOME/.bin/xdevenv night"
fi
