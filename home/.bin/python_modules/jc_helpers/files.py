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
"""Manage files and directories."""

import os
import shutil
import sys
from pathlib import Path
from typing import Callable, List, Union


def assert_commands_exist(list_commands: list):
    for cmd in list_commands:
        if not shutil.which(cmd):
            print(f"Error: the command '{cmd}' was not found.",
                  file=sys.stderr)
            sys.exit(1)


def find_in_directory_and_parents(
        list_filenames: List[str],
        start_directory: Union[os.PathLike, None] = None,
        limit_per_directory: int = -1,
        limit_directories: int = -1,
        amount_files_to_find: int = -1,
        method_check_exists: Union[Callable, None] = None) -> List[Path]:
    """
    Search for the existence of specified filenames within a directory and
    its parent directories.

    Args:
        list_filenames: A list of filenames to search for.

        start_directory: The initial directory where the search
        begins. Defaults to None (current directory).

        limit_per_directory: Limit the number of files to find per
        directory. Defaults to -1 (no limit).

        limit_directories: Limit the number of directories to search in
        (including START_DIRECTORY and its parents).
        Defaults to -1 (no limit).

        amount_files_to_find: The amount of files to find. Defaults
        to -1 (no limit).

        method_check_exists: Method that checks if a file exists. It takes
        one argument, which should be an instance of the Path class, and
        returns a boolean value.

    Returns:
        List[Path]: A list of Path objects representing the files that have
        been found within start_directory and its parent directories.

    """
    if method_check_exists is None:
        def method_check_exists(path): return Path(path).exists()

    result = []
    current_directory = Path(start_directory).resolve() \
        if start_directory else Path(".").resolve()

    while True:
        current_limit_per_directory = limit_per_directory
        for filename in list_filenames:
            path_filename = current_directory.joinpath(filename)
            if method_check_exists(path_filename):
                result.append(path_filename)

                if amount_files_to_find > 0:
                    amount_files_to_find -= 1
                    if amount_files_to_find == 0:
                        break

                if current_limit_per_directory > 0:
                    current_limit_per_directory -= 1
                    if current_limit_per_directory == 0:
                        break

        if amount_files_to_find == 0:
            break

        if limit_directories > 0:
            limit_directories -= 1
            if limit_directories == 0:
                break

        current_directory = current_directory.parent
        if current_directory in (current_directory.root,
                                 current_directory.parent):
            break

    return result
