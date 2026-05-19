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
"""Wrapper around any command."""

import logging
import os
import shlex
import shutil
import subprocess
from typing import Dict, List, Union


class CmdWrapperError(Exception):
    """Exception raised by CmdWrapper()."""


class CommandNotFound(CmdWrapperError):
    """Exception raised by CmdWrapper()."""


class CmdWrapper:
    """Wrapper around a command-line interface.

    Example:
    >>> cmd_wrapper = CmdWrapper(command_name="ls")
    >>> cmd_wrapper.run(["-l", "/etc/fstab"])
    ['/etc/fstab']

    """

    def __init__(self, command_name: str,
                 timeout: Union[None, int] = None,
                 env: Union[None, Dict[str, str]] = None):
        self._logger = logging.getLogger(command_name)
        self.timeout = timeout
        self.env = os.environ.copy() if env is None else env
        self.command_name = command_name

    def check(self):
        if not shutil.which(self.command_name):
            raise CommandNotFound(
                f"'{self.command_name}': command not found "
                f"in {os.environ['PATH']}"
            )

    def run(self, args: Union[str, List[str]], **kwargs) -> List[str]:
        """Execute the command and return the lines."""
        if isinstance(args, str):
            args = shlex.split(args)

        self._logger.debug(
            "[RUN] %s",
            subprocess.list2cmdline([self.command_name] + args),
        )

        run_kwargs = {
            "stdout": subprocess.PIPE,
            "shell": False,
            "text": True,
            "env": self.env,
            "timeout": self.timeout,
        }
        run_kwargs.update(kwargs)

        check = True
        if "check" in run_kwargs:
            check = bool(run_kwargs["check"])
            del run_kwargs["check"]

        cmd = [self.command_name] + args
        run = subprocess.run(cmd,  # type: ignore
                             check=check,
                             **run_kwargs)

        if not run.stdout:
            return []

        list_lines: List[str] = run.stdout.splitlines()
        return list_lines
