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
"""Wrapper around ddcutil."""

import logging
import re

import CmdWrapper
import CommandNotFound


class DdcutilError(Exception):
    """Exception raised by Ddcutil()."""


class Ddcutil(CmdWrapper):
    """Wrapper around 'ddcutil'."""

    # Find more capabilities: ddcutil capabilities
    # ddcutil vcpinfo 60 --verbose
    VCP_BRIGHTNESS = "10"
    VCP_MODE = "DC"

    VCP_INPUT_SOURCE = "60"

    MODE_STANDARD = "00"
    MODE_GAME = "00"

    def __init__(self):
        """Init the class Ddcutil()."""
        self._logger = logging.getLogger(self.__class__.__name__)
        super().__init__(command_name="ddcutil")
        self.list_i2c_bus = self._detect()

    def _detect(self) -> list:
        """Detect displays."""
        list_displays = []
        output = self.run(["detect"])
        for line in output:
            line = line.strip()
            match = re.match(r"^I2C bus:\s+/dev/.*-([0-9]+)$", line)
            if match:
                list_displays.append(str(match.group(1)))
        return list_displays

    def set(self, vcp: str, value: str) -> list:
        """Set a VCP (Virtual Control Panel)."""
        list_cmd = []
        cmd = []
        for i2c_bus in self.list_i2c_bus:
            cmd = ["--bus", i2c_bus, "setvcp", vcp, value]
            list_cmd.append(cmd)
            self.run(cmd)
        return cmd

    def get(self, vcp: str) -> dict:
        """Get a VCP (Virtual Control Panel)."""
        result = {}
        for i2c_bus in self.list_i2c_bus:
            cmd = ["--bus", i2c_bus, "getvcp", vcp]
            output = self.run(cmd)

            pattern = r"VCP code 0x([0-9]+) \((.*)\):\s+current value" \
                r"\s*=\s*([0-9]+),\s+max value\s*=\s*([0-9]+)"
            for line in output:
                line = line.strip()

                match = re.match(pattern, line)
                if not match:
                    continue

                item = dict(
                    vcp=str(match.group(1)),
                    vcp_str=str(match.group(2)).strip(),
                    current_value=str(match.group(3)),
                    max_value=str(match.group(4)),
                )
                result[i2c_bus] = item
                break

        return result
