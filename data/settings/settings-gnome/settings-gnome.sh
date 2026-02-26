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
IFS=$'\n\t' # strict mode

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

#-------------------------------------------------------------------->
# Background
#-------------------------------------------------------------------->
if [[ $MODIFY_BACKGROUND ]]; then
  GNOME_BACKGROUND_DIR="$USER_HOME/.backgrounds"
  mkdir -p "$GNOME_BACKGROUND_DIR"
  GNOME_BACKGROUND_PATH="$GNOME_BACKGROUND_DIR/background.png"
  cp "files-settings-gnome/background.png" "$GNOME_BACKGROUND_PATH"
fi

if [[ $MODIFY_BACKGROUND -ne 0 ]]; then
  gset org.gnome.desktop.background picture-uri "file://$GNOME_BACKGROUND_PATH"
fi
#-------------------------------------------------------------------->

gset org.gnome.system.locale region en_US.UTF-8
# gset org.gnome.system.locale region en_CA.UTF-8

# The Colon (:): This character acts as a separator between the left and right
# sides of the title bar.
# gsettings set org.gnome.desktop.wm.preferences button-layout 'close,minimize,maximize:'
gsettings set org.gnome.desktop.wm.preferences button-layout 'close:'

gset org.gnome.desktop.interface cursor-theme Adwaita
gset org.gnome.desktop.interface document-font-name 'DejaVu Sans 10'
gset org.gnome.desktop.interface font-name 'DejaVu Sans 10'
gset org.gnome.desktop.interface monospace-font-name 'Inconsolata Bold 10'
gset org.gnome.desktop.interface font-antialiasing 'rgba' # rgba / grayscale
gset org.gnome.desktop.interface font-hinting 'medium'    # slight / medium / full
gset org.gnome.desktop.interface gtk-theme Adwaita-dark   # HighContrastInverse / Adwaita-dark
gset org.gnome.desktop.wm.preferences titlebar-font 'DejaVu Sans 10'
gset org.gnome.desktop.interface icon-theme Adwaita

if type -P meld >/dev/null 2>&1; then
  gset org.gnome.meld custom-font 'monospace 16'
fi

gset org.gnome.desktop.input-sources xkb-options \
  "['caps:none', 'numpad:mac', 'compose:ralt']"

gset org.gnome.software allow-updates false || true
gset org.gnome.software download-updates false || true

gset org.gnome.settings-daemon.plugins.media-keys volume-step 1

# gset org.gnome.shell disable-user-extensions true

# Arch Linux font
# TODO: Check first if the schema org.gnome.Terminal.ProfilesList exists.
#
# if pacman -Q ttf-inconsolata >/dev/null 2>&1; then
# if [[ -f "/usr/share/fonts/TTF/Inconsolata-UltraCondensed.ttf" ]]; then
#   gset_terminal font 'Inconsolata Ultra-Bold 12'
#   gset_terminal use-system-font false
# else
#   gset_terminal use-system-font true
# fi
# fi

# gset_terminal font 'Monospace 10'
# gset_terminal palette "['rgb(0,0,0)', 'rgb(170,0,0)', 'rgb(0,170,0)', 'rgb(170,85,0)', 'rgb(0,0,170)', 'rgb(170,0,170)', 'rgb(0,170,170)', 'rgb(170,170,170)', 'rgb(85,85,85)', 'rgb(255,85,85)', 'rgb(85,255,85)', 'rgb(255,255,85)', 'rgb(85,85,255)', 'rgb(255,85,255)', 'rgb(85,255,255)', 'rgb(255,255,255)']"
# gset_terminal bold-is-bright true
# gset_terminal audible-bell false
# gset_terminal use-theme-colors false
# gset_terminal background-color '#000000'
# gset_terminal foreground-color '#DFDFDF'
# gset_terminal scrollbar-policy never
