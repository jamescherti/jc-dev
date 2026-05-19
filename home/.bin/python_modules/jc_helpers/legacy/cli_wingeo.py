#!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Copyright (C) 2004-2026 James Cherti
#
# This program is free software: you can redistribute it and/or modify it under
# the terms of the GNU General Public License as published by the Free Software
# Foundation, either version 3 of the License, or (at your option) any later
# version.
#
# This program is distributed in the hope that it will be useful, but WITHOUT
# ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS
# FOR A PARTICULAR PURPOSE. See the GNU General Public License for more
# details.
#
# You should have received a copy of the GNU General Public License along with
# this program. If not, see <https://www.gnu.org/licenses/>.
#
"""Save and restore the windows position."""

import argparse
import json
import logging
import os
import re
import subprocess
import sys
from pathlib import Path
from typing import Dict, List, Union

from pydantic import BaseModel


class WindowGeoError(Exception):
    """Base exception for WindowGeo classes."""


class Geometry(BaseModel):
    """Position and size."""
    x: int
    y: int
    width: int
    height: int

    def equals(self, geometry: "Geometry"):
        return bool((self.x, self.y, self.width, self.height) ==
                    (geometry.x, geometry.y, geometry.width, geometry.height))


class WindowInfo(BaseModel):
    """Window information."""
    win_id: str
    wm_class: str
    geometry: Geometry
    wm_state: list

    def equals(self, window_info: "WindowInfo"):
        return bool(
            (self.win_id,
             self.wm_class) ==
            (window_info.win_id,
             window_info.wm_class)
        )

    def is_maximized(self,
                     horizontal=True,
                     vertical=True,
                     fullscreen=True) -> bool:
        """Return True if the Window is maximized or fullscreen."""
        checks = []
        if horizontal:
            checks += ["_NET_WM_STATE_MAXIMIZED_HORZ"]
        if vertical:
            checks += ["_NET_WM_STATE_MAXIMIZED_VERT"]
        if fullscreen:
            checks += ["_NET_WM_STATE_FULLSCREEN"]
        for check in checks:
            if check in self.wm_state:
                return True
        return False


class Xwrappers:
    """Wrapper around 'xdotool' and 'xprop'."""

    os_env = os.environ.copy()
    os_env["LC_ALL"] = "C"

    @staticmethod
    def run(*args):
        output = subprocess.check_output(list(args),
                                         env=Xwrappers.os_env,
                                         stderr=subprocess.DEVNULL)
        return output.decode("utf-8").splitlines()

    @staticmethod
    def get_xproperties(win_id: str):
        list_vars: dict = {}
        for line in Xwrappers.run("xprop", "-id", win_id):
            match = re.match(r"^(.*)\(.*\)\s+=\s+(.*)$", line.strip())
            if match:
                list_vars[str(match.group(1)).strip()] = \
                    str(match.group(2).strip())

        if "_NET_WM_STATE" in list_vars:
            list_vars["_NET_WM_STATE"] = [
                wm_state.strip()
                for wm_state in list_vars["_NET_WM_STATE"].split(",")
            ]
        else:
            list_vars["_NET_WM_STATE"] = []

        if "_NET_FRAME_EXTENTS" in list_vars:
            net_frame_extends_list = [
                wm_state.strip()
                for wm_state in list_vars["_NET_FRAME_EXTENTS"].split(",")
            ]

            list_vars["_NET_FRAME_EXTENTS"] = {
                "left": int(net_frame_extends_list[0]),
                "right": int(net_frame_extends_list[1]),
                "top": int(net_frame_extends_list[2]),
                "bottom": int(net_frame_extends_list[3]),
            }
        else:
            list_vars["_NET_FRAME_EXTENTS"] = {"left": 0,
                                               "right": 0,
                                               "top": 0,
                                               "bottom": 0}

        return list_vars

    @staticmethod
    def set_geometry(win_id: str, geometry: Geometry):
        # Change position
        Xwrappers.run("xdotool", "windowmove", win_id,
                      str(geometry.x), str(geometry.y))
        Xwrappers.run("xdotool", "windowsize", win_id,
                      str(geometry.width), str(geometry.height))

    @staticmethod
    def get_geometry(win_id) -> Geometry:
        # return get_geometry_xdotool(win_id)
        return Xwrappers.get_geometry_xwininfo(win_id)

    @staticmethod
    def get_geometry_xwininfo(win_id) -> Geometry:
        """return the window geometry."""

        list_var_names = ["Absolute upper-left X", "Absolute upper-left Y",
                          "Width", "Height"]
        geometry: Dict[str, str] = {}

        output = Xwrappers.run("xwininfo", "-id", win_id)
        for line in output:
            line = line.strip()
            for var_name in list_var_names:
                if line.startswith(f"{var_name}:"):
                    geometry[var_name] = line[len(var_name) + 1:]

        if set(geometry.keys()) != set(list_var_names):
            raise WindowGeoError("invalid output returned by "
                                 "'xdotool getwindowgeometry'. Output: " +
                                 str(output))

        return Geometry(x=int(geometry["Absolute upper-left X"]),
                        y=int(geometry["Absolute upper-left Y"]),
                        width=int(geometry["Width"]),
                        height=int(geometry["Height"]))

    @staticmethod
    def get_geometry_xdotool(win_id) -> Geometry:
        """return geometry."""
        list_var_names = ["X", "Y", "WIDTH", "HEIGHT", "SCREEN"]
        geometry: Dict[str, str] = {}

        output = Xwrappers.run(
            "xdotool", "getwindowgeometry", "--shell", win_id)
        for line in output:
            line = line.strip()
            for var_name in list_var_names:
                if line.startswith(f"{var_name}="):
                    geometry[var_name] = line[len(var_name) + 1:]

        if set(geometry.keys()) != set(list_var_names):
            raise WindowGeoError("invalid output returned by "
                                 "'xdotool getwindowgeometry'. Output: " +
                                 str(output))

        return Geometry(x=int(geometry["X"]),
                        y=int(geometry["Y"]),
                        width=int(geometry["WIDTH"]),
                        height=int(geometry["HEIGHT"]))

    @staticmethod
    def is_normal_window(wm_state: list) -> bool:
        """Return True when the window is normal (e.g. not task bar)."""
        for item in wm_state:
            if item in {"_NET_WM_STATE_SKIP_PAGER",
                        "_NET_WM_STATE_SKIP_TASKBAR"}:
                return False
        return True

    @staticmethod
    def find_windows_xdotool() -> Dict[str, WindowInfo]:
        """Return all visible Windows."""
        window_info = {}
        for win_id in Xwrappers.run("xdotool", "search",
                                    "--onlyvisible", ".*"):
            win_id = win_id.strip()
            if win_id == "":
                continue

            xprop_vars = Xwrappers.get_xproperties(win_id)
            if "_NET_WM_WINDOW_TYPE" not in xprop_vars:
                continue

            __import__('pprint').pprint(xprop_vars)
            if not Xwrappers.is_normal_window(xprop_vars["_NET_WM_STATE"]):
                continue

            if xprop_vars["_NET_WM_WINDOW_TYPE"] != \
                    "_NET_WM_WINDOW_TYPE_NORMAL":
                continue

            try:
                wm_class = xprop_vars["WM_CLASS"]
            except KeyError:
                wm_class = ""

            if wm_class == "":
                err_str = f"Cannot read the WM_CLASS of the Window {win_id}"
                raise WindowGeoError(err_str)

            wm_class = wm_class.replace(" ", "").replace(",", ".") \
                .replace("\"", "")

            geometry = Xwrappers.get_geometry(win_id)
            kwargs = {
                "win_id": win_id,
                "geometry": Geometry(
                    x=geometry.x - xprop_vars["_NET_FRAME_EXTENTS"]["left"],
                    y=geometry.y - xprop_vars["_NET_FRAME_EXTENTS"]["top"],
                    width=geometry.width,
                    height=geometry.height,
                ),
                "wm_class": wm_class,
                "wm_state": xprop_vars["_NET_WM_STATE"],
            }

            window_info[win_id] = WindowInfo(**kwargs)

        return window_info

    @staticmethod
    def find_windows_wmctrl(whitelist_wm_class: Union[list, None] = None) \
            -> Dict[str, WindowInfo]:
        """Return all visible Windows."""
        windows_info: dict = {}
        for item in Xwrappers.run("wmctrl", "-lxG"):
            item = item.split()
            win_id = item[0]

            if win_id in windows_info:
                raise WindowGeoError("invalid output returned by 'wmctrl'.")

            wm_class = item[6]
            if whitelist_wm_class and wm_class not in whitelist_wm_class:
                continue

            xprop_vars = Xwrappers.get_xproperties(win_id)
            if not Xwrappers.is_normal_window(xprop_vars["_NET_WM_STATE"]):
                continue

            geometry = Xwrappers.get_geometry(win_id)  # Fix wmctrl geometry
            kwargs = {
                "win_id": win_id,
                "geometry": Geometry(
                    x=geometry.x - xprop_vars["_NET_FRAME_EXTENTS"]["left"],
                    y=geometry.y - xprop_vars["_NET_FRAME_EXTENTS"]["top"],
                    width=geometry.width,
                    height=geometry.height,
                ),
                "wm_class": wm_class,
                "wm_state": xprop_vars["_NET_WM_STATE"],
            }

            windows_info[win_id] = WindowInfo(**kwargs)

        return windows_info


class ListWindows:
    """List of Windows information."""

    json_kwargs = {"indent": 2}
    data_key = "windows_info"

    def __init__(self, detect=True,
                 whitelist_wm_class: Union[list, None] = None):
        self.whitelist_wm_class = []
        if whitelist_wm_class:
            self.whitelist_wm_class = whitelist_wm_class

        self.windows_info: Dict[str, WindowInfo] = {}
        if detect:
            self.update()

    def update(self):
        """Reload the Windows information."""
        # self.windows_info = Xwrappers.find_windows_xdotool()
        self.windows_info = Xwrappers.find_windows_wmctrl(
            whitelist_wm_class=self.whitelist_wm_class
        )

    def save_json(self, path: Union[Path, str]):
        """Save the Windows information to a file."""
        with open(path, "w", encoding="utf-8") as fhandler:
            fhandler.write(self.dump_json())

    def dump_json(self):
        """Dump the Windows information."""
        data = {self.data_key:
                {key: value.dict()
                 for key, value in self.windows_info.items()}}
        return json.dumps(data, **ListWindows.json_kwargs)

    def load_json(self, path: Union[Path, str]):
        """Load the Windows information from a file."""
        self.windows_info = {}
        with open(path, "r", encoding="utf-8") as fhandler:
            self.load_json_from_string(fhandler.read())

    def load_json_from_string(self, string: str):
        """Load from string"""
        data = json.loads(string)
        for win_id, window_info_dict in data[self.data_key].items():
            self.windows_info[win_id] = WindowInfo(**window_info_dict)

    def restore_geometry(self, wm_class: Union[None, list] = None):
        """Restore the geometry of the Windows (x, y, width, height)."""
        cur_win_info = ListWindows(whitelist_wm_class=self.whitelist_wm_class)

        for win_id, old_win_info in self.windows_info.items():
            if wm_class is not None and old_win_info.wm_class not in wm_class:
                logging.debug("Ignored (WM Class different): %s",
                              str(old_win_info))
                continue

            if win_id not in cur_win_info.windows_info or \
                    not old_win_info.equals(cur_win_info.windows_info[win_id]):
                new_window = cur_win_info.find(old_win_info.wm_class)
                if new_window:
                    logging.debug("New win ID for %s: %s",
                                  old_win_info.wm_class, str(new_window))

                    win_id = new_window[0].win_id
                    old_win_info.win_id = win_id
                else:
                    logging.debug("Ignored (window closed): %s",
                                  str(old_win_info))
                    continue

            if old_win_info.is_maximized():
                logging.debug("Ignored (maximized or fullscreen): %s",
                              str(old_win_info))
                continue

            if old_win_info.geometry.equals(cur_win_info.windows_info[win_id]
                                            .geometry):
                logging.debug("Ignored (geometry and position are equal): %s",
                              str(old_win_info))
                continue

            # Unfullscreen
            if cur_win_info.windows_info[win_id] \
                    .is_maximized(horizontal=False, vertical=False):
                logging.debug("Unfullscreen: %s", str(win_id))
                cmd = ["wmctrl", "-i", "-r", win_id,
                       "-b", "remove,fullscreen"]
                logging.debug("[CMD] %s", str(cmd))
                # logging.debug("[CMD-STR] %s", " ".join(cmd))
                Xwrappers.run(*cmd)

            # Unmaximize
            if cur_win_info.windows_info[win_id] \
                    .is_maximized(fullscreen=False):
                logging.debug("Unmaximize: %s", str(win_id))
                cmd = ["wmctrl", "-i", "-r", win_id,
                       "-b", "remove,maximized_vert,maximized_horz"]
                logging.debug("[CMD] %s", str(cmd))
                # logging.debug("[CMD-STR] %s", " ".join(cmd))
                Xwrappers.run(*cmd)

            # Set geometry
            logging.debug("Update geometry from '%s'",
                          str(cur_win_info.windows_info[win_id]))
            logging.debug("                to   '%s'",
                          str(old_win_info))
            Xwrappers.set_geometry(
                win_id=old_win_info.win_id,
                geometry=Geometry(
                    x=old_win_info.geometry.x,
                    y=old_win_info.geometry.y,
                    width=old_win_info.geometry.width,
                    height=old_win_info.geometry.height
                )
            )

    def find(self, wm_class: Union[str, List[str]]):
        if isinstance(wm_class, str):
            wm_class = [wm_class]

        result = []
        for _, win_info in self.windows_info.items():
            if win_info.wm_class in wm_class:
                result.append(win_info)

        return result


def parse_args():
    """Parse the command line arguments."""
    desc = __doc__
    usage = "%(prog)s [--option] [args]"
    parser = argparse.ArgumentParser(description=desc, usage=usage)
    parser.add_argument("filename",
                        type=str,
                        nargs="?",
                        help="Json file (dash '-' for stdin)")
    parser.add_argument("-c", "--wmclass",
                        type=str,
                        action="append",
                        help="Only save / load a specific WM_CLASS")
    args = parser.parse_args()
    return args


def command_line_interface():
    """Command-line interface."""
    logging.basicConfig(level=logging.INFO, stream=sys.stdout,
                        format="%(asctime)s %(name)s: %(message)s")

    args = parse_args()

    windows = ListWindows(whitelist_wm_class=args.wmclass)
    if args.filename is not None:
        if args.filename == "-":
            content = sys.stdin.read()
        else:
            with open(args.filename, "r", encoding="utf-8") as fhandler:
                content = fhandler.read()

        windows.load_json_from_string(content)
        windows.restore_geometry()
    else:
        dump = windows.dump_json()
        print(dump)


if __name__ == '__main__':
    command_line_interface()
