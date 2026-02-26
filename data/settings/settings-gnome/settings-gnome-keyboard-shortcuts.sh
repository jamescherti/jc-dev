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

#
# GNOME Settings
#
# TODO: reset tracker
# systemctl --user mask tracker-store.service tracker-miner-fs.service tracker-miner-rss.service tracker-extract.service tracker-miner-apps.service tracker-writeback.service
# tracker reset --hard
# https://www.linuxuprising.com/2019/07/how-to-completely-disable-tracker.html
#

set -euf -o pipefail
IFS=$'\n\t' # strict mode

SHORTCUT_INDEX=0
SHORTCUT_STRING=""
MODIFY_BACKGROUND=1

USER_HOME="$(eval echo "~$USER")"

run() {
  echo "$@"
  "$@"
}

gset() {
  run gsettings set "$@" || return 1
  return 0
}

shortcut() {
  local shortcut_name="'$1'"
  local shortcut="'$2'"
  local shortcut_cmd="'$3'"

  local uri="org.gnome.settings-daemon.plugins.media-keys.custom-keybinding:/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom$SHORTCUT_INDEX/"
  run gsettings set "$uri" name "$shortcut_name"
  run gsettings set "$uri" binding "$shortcut"
  run gsettings set "$uri" command "$shortcut_cmd"

  if [[ $SHORTCUT_STRING != "" ]]; then
    SHORTCUT_STRING="${SHORTCUT_STRING},"
  fi
  SHORTCUT_STRING="${SHORTCUT_STRING}'/org/gnome/settings-daemon/plugins/media-keys/custom-keybindings/custom${SHORTCUT_INDEX}/'"

  ((SHORTCUT_INDEX += 1))
}

if [[ $USER = "work" ]]; then
  shortcut "xocrshot" "<Primary><Alt>O" "$USER_HOME/.bin/xdevenv xocrshot"
  # shortcut "Screenshot" "<Primary><Alt>C" "$USER_HOME/.bin/xdevenv screenshot"
  shortcut "Editor" "<Primary><Alt>V" "$USER_HOME/.bin/xdevenv editor"
  shortcut "Terminal" "<Primary><Alt>R" "$USER_HOME/.bin/xdevenv terminal"
  shortcut "Web Browser" "<Primary><Alt>F" "$USER_HOME/.bin/xdevenv web-browser"
  shortcut "Pwd Manager" "<Primary><Alt>S" "$USER_HOME/.bin/xdevenv pwd-manager"
  shortcut "Mail Client" "<Primary><Alt>Q" "$USER_HOME/.bin/xdevenv mail-client"
  shortcut "Suspend" "<Primary>F12" "dbus-send --system --print-reply --dest=org.freedesktop.login1 /org/freedesktop/login1 org.freedesktop.login1.Manager.Suspend boolean:true"
  shortcut "File Explorer" "<Primary><Alt>E" "$USER_HOME/.bin/xdevenv file-explorer"
  shortcut "Firefox Profiles" "<Primary><Alt>P" "$USER_HOME/.bin/xdevenv web-browser-profiles"
  shortcut "Changer user" "<Primary><Alt>U" "$USER_HOME/.bin/xdevenv gdm-change-user"
  shortcut "Light" "<Primary><Alt>M" "$USER_HOME/.bin/xdevenv light"
  shortcut "Night" "<Primary><Alt>N" "$USER_HOME/.bin/xdevenv night"
fi
