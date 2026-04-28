#!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Copyright (C) 2004-2026 James Cherti
#
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
"""Grep any file."""

import os
import subprocess
import sys

import colorama
import setproctitle
from colorama import Fore

from readtext import ReadText


def del_opt(arg, long_opts, short_opts):
    """Remove an option from an argument.

    >>> del_opt("-iVm", ["--hello"], ["V"])
    "-im"

    """
    if arg.startswith("--"):
        try:
            equal_index = arg.index("=")
        except ValueError:
            # equal not found
            pass
        else:
            arg = arg[0:equal_index]

        if arg in long_opts:
            return ""
    elif arg.startswith("-"):
        new_arg = ""
        for char in arg[1:]:
            if char not in short_opts:
                new_arg += char

        if new_arg:
            new_arg = "-" + new_arg
        return new_arg
    return arg


def separate_args_files(args):
    """Separate arguments from files."""
    args_opts = []
    args_files = []
    double_dash_found = False
    for arg in args:
        if double_dash_found:
            args_files.append(arg)
            continue

        if arg == "--":
            double_dash_found = True
            continue

        if arg.startswith("-"):
            args_opts.append(arg)
        else:
            args_files.append(arg)
    return (args_opts, args_files)


# pylint: disable=too-many-statements
# pylint: disable=too-many-branches
# pylint: disable=too-many-locals
def cmd_grep(rtext, grep_args):
    """Grep readtext files."""
    # Init vars
    color = False
    exit_code = 0
    recursive = False
    followlinks = False
    grep_args_opts, grep_args_files = separate_args_files(grep_args)
    try:
        grep_keyword = grep_args_files[0]
    except IndexError:
        print(f"Usage: {sys.argv[0]} grep <PATTERN> "
              "[file1] [file2]...",
              file=sys.stderr)
        sys.exit(1)

    grep_args_files = grep_args_files[1:]

    if hasattr(sys.stdout, "isatty") and sys.stdout.isatty():
        color = True

    # remove some options from the arguments
    _tmp_table = []
    for grep_arg in grep_args_opts:
        del_opts_recur = del_opt(grep_arg, ["--recursive"], ["r"])
        del_opts_recur_flink = del_opt(grep_arg,
                                       ["--dereference-recursive"], ["R"])
        # pylint: disable=no-else-continue
        if grep_arg != del_opts_recur or grep_arg != del_opts_recur_flink:
            recursive = True
            if grep_arg != del_opts_recur_flink:
                # in case it is del_opts_recur_flink
                # -r
                followlinks = True
                if del_opts_recur_flink:
                    _tmp_table.append(del_opts_recur_flink)
            else:
                # in case it is del_opts_recur
                # -R
                if del_opts_recur:
                    _tmp_table.append(del_opts_recur)

            continue
        else:
            if del_opts_recur:
                _tmp_table.append(grep_arg)

            # some exceptions
            if grep_arg.startswith("--color"):
                # user specified color
                color = False
    grep_args_opts = _tmp_table

    # recursive
    if not grep_args_files:
        recursive = True
        grep_args_files = ["."]

    if not recursive:
        file_list = grep_args_files
    else:
        file_list = []
        for item in grep_args_files:
            if os.path.isfile(item):
                file_list.append(item)
            else:
                # recursive scan
                for folder, _, sub_files in os.walk(item,
                                                    followlinks=followlinks):
                    sub_files = [os.path.join(folder, sub_file)
                                 for sub_file in sub_files]
                    for path_file in sub_files:
                        file_list.append(path_file)

    # start scanning the files
    if color:
        grep_args_opts.insert(0, "--color=always")

    for filename in file_list:
        text = rtext.load(filename)

        cmd = ["grep"] + grep_args_opts + [grep_keyword]
        # print("[CMD] {}".format(" ".join(cmd)), file=sys.stderr)
        # pylint: disable=consider-using-with
        proc = subprocess.Popen(cmd,
                                stdout=subprocess.PIPE,
                                stderr=subprocess.STDOUT,
                                stdin=subprocess.PIPE)
        (stdout, _) = proc.communicate(input=text.encode())
        if proc.returncode != 0:
            exit_code = 1

        stdout = stdout.decode()
        for line in stdout.splitlines():
            # pylint: disable=consider-using-f-string
            print("{} {}".format(filename + ":" if not color else
                                 Fore.YELLOW + (filename + ":"),
                                 line))

    sys.exit(exit_code)


def command_line_interface():
    """The command line interface of readtext."""
    setproctitle.setproctitle(os.path.basename(sys.argv[0]))
    colorama.init()

    rtext = ReadText()

    # valid commands: grep, cat...
    try:
        command = sys.argv[1]
    except IndexError:
        print(f"Usage: {sys.argv[0]} <cat|grep> ...", file=sys.stderr)
        sys.exit(1)

    if command == "grep":
        cmd_grep(rtext, sys.argv[2:])
    elif command == "cat":
        exit_code = 0
        for filename in sys.argv[2:]:
            text = rtext.load(filename)
            print(text)
        sys.exit(exit_code)
    else:
        print(f"Error: invalid command '{command}'.",
              file=sys.stderr)
        sys.exit(1)

    sys.exit(0)


if __name__ == "__main__":
    command_line_interface()
