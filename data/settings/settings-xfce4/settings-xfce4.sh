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
# xfce4-keyboard-shortcuts
#
if type -P xdevenv &>/dev/null; then
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Alt>F4'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Primary><Alt>z' --type 'string' --set 'close_window_key'

  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>x' --type 'string' --set "$HOME/.bin/xdevenv mpv"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>f' --type 'string' --set "$HOME/.bin/xdevenv web-browser"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>p' --type 'string' --set "$HOME/.bin/xdevenv web-browser-profiles"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>s' --type 'string' --set "$HOME/.bin/xdevenv pwd-manager"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>c' --type 'string' --set "$HOME/.bin/xdevenv screenshot"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>v' --type 'string' --set "$HOME/.bin/xdevenv editor"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>e' --type 'string' --set "$HOME/.bin/xdevenv file-explorer"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/commands/custom/<Primary><Alt>w' --type 'string' \
    --set "xfce4-appfinder"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>q' --type 'string' --set "$HOME/.bin/xdevenv mail-client"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>r' --type 'string' --set "$HOME/.bin/xdevenv terminal"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>n' --type 'string' --set "$HOME/.bin/xdevenv night"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>m' --type 'string' --set "$HOME/.bin/xdevenv light"

  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>h'

  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>o' --type 'string' --set "$HOME/.local/bin/xocrshot"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Super>F12' --type 'string' --set "$HOME/.bin/xdevenv suspend"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Shift>Delete' --type 'string' --set 'true' # Force user to use trash (Thunar)

  # xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/AudioLowerVolume' --type 'string' --set "$HOME/.bin/xdevenv volume-down"
  # xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/AudioRaiseVolume' --type 'string' --set "$HOME/.bin/xdevenv volume-up"
  # xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/AudioMute' --type 'string' --set "$HOME/.bin/xdevenv volume-mute"
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>y' --type 'string' --set "$HOME/.bin/xdevenv pavucontrol"
  # xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>y' --type 'string' --set "$HOME/.bin/xdevenv xfce-display1"
  # xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Primary><Alt>u' --type 'string' --set "$HOME/.bin/xdevenv xfce-display2"

  # Hjkl
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Super>h' --type 'string' \
    --set 'tile_left_key'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Super>l' --type 'string' \
    --set 'tile_right_key'

  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Super>Up'
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Super>Down'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Super>k' --type 'string' \
    --set 'maximize_window_key'

  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Alt>F10' # maximize_window_key

  # move_window_left_workspace_key
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Shift><Alt>Left'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Shift><Alt>h' \
    --type 'string' --set 'move_window_left_workspace_key'

  # minimize
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Alt>F9'
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Alt>j'
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Super>j'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Alt>d' --type 'string' \
    --set 'hide_window_key' # Minimize

  # move_window_right_workspace_key
  xfconf-query --reset -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Shift><Alt>Right'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Shift><Alt>l' \
    --type 'string' --set 'move_window_right_workspace_key'

  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Alt>h' --type 'string' \
    --set 'left_workspace_key' # Switch windows for the same application

  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Alt>l' --type 'string' \
    --set 'right_workspace_key' # Switch windows for the same application
else
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Shift><Alt>Left' --type 'string' \
    --set 'move_window_left_workspace_key'
  xfconf-query --create -c 'xfce4-keyboard-shortcuts' \
    -p '/xfwm4/custom/<Primary><Shift><Alt>Right' --type 'string' \
    --set 'move_window_right_workspace_key'
fi

# xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Super>l' --type 'string' --set 'xflock4'
# xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Super>s' --type 'string' --set 'xflock4'
# xfconf-query --reset -c 'xfce4-keyboard-shortcuts' -p '/commands/custom/<Super>l'

# shellcheck disable=SC2041
for item in '<Primary><Alt>l'; do
  xfconf-query -r -c 'xfce4-keyboard-shortcuts' -p "/commands/custom/$item"
done

# If you wish to list them: xfconf-query --list -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom'
for item in '<Super>Tab' '<Alt>space' '<Super>KP_Down' '<Super>KP_End' '<Super>KP_Home' '<Super>KP_Left' '<Super>KP_Next' '<Super>KP_Page_Up' '<Super>KP_Right' '<Super>KP_Up'; do
  xfconf-query -r -c 'xfce4-keyboard-shortcuts' -p "/xfwm4/custom/$item"
done

xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Alt>grave' --type 'string' --set 'switch_window_key' # Switch windows for the same application
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Down' --type 'string' --set 'tile_down_key'
# xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Up' --type 'string' --set 'maximize_window_key'  # before: tile_up_key
# xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Left' --type 'string' --set 'tile_left_key'
# xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Right' --type 'string' --set 'tile_right_key'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Home' --type 'string' --set 'tile_up_left_key'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Page_Up' --type 'string' --set 'tile_up_right_key'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>End' --type 'string' --set 'tile_down_left_key'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Super>Page_Down' --type 'string' --set 'tile_down_right_key'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Primary><Shift><Alt>Down' --type 'string' --set 'move_window_down_workspace_key'
xfconf-query --create -c 'xfce4-keyboard-shortcuts' -p '/xfwm4/custom/<Primary><Shift><Alt>Up' --type 'string' --set 'maximize'

# TODO: write a wrapper
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/inactivity-on-ac' --type 'uint' --set '14'                # ac: suspend after x minutes
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/inactivity-sleep-mode-on-battery' --type 'uint' --set '1' # 1=suspend 2=hibernate
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/inactivity-sleep-mode-on-ac' --type 'uint' --set '1'      # 1=suspend 2=hibernate

xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/blank-on-battery' --type 'int' --set '1' # minutes

# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-ac-sleep' --type 'uint' --set '58' # put display to sleep after x minutes
# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/blank-on-ac' --type 'int' --set '15'           # minutes
# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-ac-off' --type 'uint' --set '59'   # disabled. Do not switch off (curved mon)

xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-ac-sleep' --type 'uint' --set '3'
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/blank-on-ac' --type 'int' --set '2'
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-ac-off' --type 'uint' --set '5'
# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-ac-off' --type 'uint' --set '20'         # put display to sleep after x minutes
# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-ac-sleep' --type 'uint' --set '17'       # put display to sleep after x minutes

xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-battery-off' --type 'uint' --set '3'     # put display to sleep after x minutes
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/dpms-on-battery-sleep' --type 'uint' --set '2'   # put display to sleep after x minutes
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/hibernate-button-action' --type 'uint' --set '1' # 0=do_nothing 1=suspend 2=hibernate 3=ask 4=shutdown

xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/critical-power-level' --type 'uint' --set '2'
xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/critical-power-level' --type 'uint' --set '2'

# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/lock-screen-suspend-hibernate' --type 'bool' --set 'true'
# fi

# xfconf-query --create -c 'xfce4-power-manager' \
#   -p '/xfce4-power-manager/critical-power-level' \
#   --type 'uint' --set '6' # percentage to trigger critical battery power
# xfconf-query --create -c 'xfce4-power-manager' -p '/xfce4-power-manager/critical-power-action' --type 'uint' --set '1' # What should XFCE do when the battery power is critical? 1=suspend 0=nothing
# xfconf-query --create -c 'xfce4-notifyd' \
#   -p '/applications/muted_applications' --type 'string' --set 'Power Manager'
# xfconf-query --create -c 'xfce4-notifyd' \
#  -p '/notify-location' --type 'uint' --set '1'  # 1=bottom left

#
# xsettings
#

# xfconf-query --create -c 'xsettings' \
#   -p '/Net/ThemeName' --type 'string' --set 'Adwaita-dark'

# xfconf-query --create -c 'xsettings' \
#   -p '/Net/IconThemeName' --type 'string' --set 'Adwaita'

# Windows buttons (O=options H=minimize M=maximize C=close)
# xfconf-query --create -c 'xfwm4' \
#   -p '/general/button_layout' --type 'string' --set 'O|HMC'

# xfconf-query --create -c 'xfwm4' \
#   -p '/general/use_compositing' --type 'bool' --set 'false'
# xfconf-query --create -c 'xfwm4' \
#   -p '/general/frame_opacity' --type 'int' --set '0'

# xfconf-query --create -c 'xfwm4' -p '/general/theme' --type 'string' --set 'Default'
# xfconf-query --create -c 'xsettings' -p '/Gtk/DecorationLayout' --type 'string' --set 'close:'
# xfconf-query --create -c 'xfwm4' -p '/general/button_layout' --type 'string' --set 'C|'
# xfconf-query --create -c 'xfwm4' -p '/general/placement_ratio' --type 'int' --set '15'
# xfconf-query --create -c 'xfwm4' \
#   -p '/general/cycle_tabwin_mode' --type 'int' --set '1' # cycle Windows in a list
