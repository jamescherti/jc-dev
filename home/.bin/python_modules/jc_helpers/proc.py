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
"""Processors managers."""


import getpass
import os
import re
from typing import Generator, List, Set

import psutil


class FindProc:
    """Return all processes."""

    def __init__(self, all_users: bool = False):
        """
        Initialize the instance.

        Args:
            all_users: If True, include processes of all users; if False, only
            current user's processes.

        """
        self.all_users = all_users
        self.current_user = getpass.getuser()

    def __iter__(self) -> Generator[psutil.Process, None, None]:
        """Iterate over processes."""
        return self._all()

    def to_list(self) -> List[psutil.Process]:
        """Return processes as a list."""
        return list(self._all())

    def exists(self) -> bool:
        return bool(self.to_list())

    def _all(self) -> Generator[psutil.Process, None, None]:
        """Generator that yields processes."""
        for pid in psutil.pids():
            try:
                proc = psutil.Process(pid)
            except psutil.Error:
                continue

            if not self.all_users and proc.username() != self.current_user:
                continue

            yield proc

    def __repr__(self):
        return str(self.to_list())

    def _process_names(self, proc: psutil.Process) -> Set[str]:
        """Get the names of a process.

        Args:
            proc: The process.

        Returns:
            List of process names.
        """
        result = set()
        try:
            result.add(proc.cmdline()[0])
            result.add(os.path.basename(proc.cmdline()[0]))
        except IndexError:
            pass

        proc_name = proc.name()
        result.add(proc_name)
        result.add(os.path.basename(proc_name))

        return result


class FindProcName(FindProc):
    def __init__(self, name: str, all_users: bool = False):
        """Initialize the instance.

        Args:
            name: Process name (exact).
            all_users: If True, include processes of all users; if False, only
            current user's processes.
        """
        super().__init__(all_users=all_users)
        self.name = name  # Process name (exact)

    def _all(self) -> Generator[psutil.Process, None, None]:
        for proc in super()._all():
            for cur_name in self._process_names(proc):
                if self.name and self.name == os.path.basename(cur_name):
                    yield proc
                    break


class FindProcRegexCmdline(FindProc):
    """Return processes by cmdline regular expression matching."""

    def __init__(self, regex: str = "",
                 regex_flags: int = 0,
                 all_users: bool = False):
        """Initialize the instance.

        Args:
            regex: Regular expression for command line.
            regex_flags: Regular expression flags.
            all_users: If True, include processes of all users; if False, only
            current user's processes.
        """
        super().__init__(all_users=all_users)
        self.regex = regex
        self.regex_flags = regex_flags

    def _all(self):
        """Generator that yields processes matching regular expressions."""
        for proc in super()._all():
            if self.regex and re.search(self.regex,
                                        " ".join(proc.cmdline()),
                                        self.regex_flags):
                yield proc


class FindProcRegexName(FindProc):
    """Return processes by regular expression matching."""

    def __init__(self, regex: str = "",
                 regex_flags: int = 0,
                 all_users: bool = False):
        """Initialize the instance.

        Args:
            regex: Regular expression for process name.
            regex_flags: Regular expression flags.
            all_users: If True, include processes of all users; if False, only
            current user's processes.
        """
        super().__init__(all_users=all_users)
        self.regex = regex
        self.regex_flags = regex_flags

    def _all(self):
        """Generator that yields processes matching regular expressions."""
        for proc in super()._all():
            found = False
            for cur_name in self._process_names(proc):
                if self.regex and \
                        re.search(self.regex, cur_name, self.regex_flags):
                    found = True
                    break

            if found:
                yield proc
