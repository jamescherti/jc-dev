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
"""Extract dates from a string of text."""

import sys
# pylint: disable=wrong-import-order
import warnings
from datetime import datetime
from pathlib import Path
from shutil import move

import setproctitle
from colorama import Fore

from readtext import ReadText

# from dateparser.search import search_dates  # type: ignore
# from dateparser_data.settings import default_parsers


def command_line_interface():
    """Command line interface."""
    warnings.filterwarnings("ignore")

    setproctitle.setproctitle(Path(sys.argv[0]).name)

    readtext = ReadText()

    for path in sys.argv[1:]:
        path = Path(path)
        output = readtext.load(path)

        result = []

        filename_stat = Path(path.name).stat()
        filename_ctime = datetime.fromtimestamp(filename_stat.st_mtime)
        filename_mtime = datetime.fromtimestamp(filename_stat.st_ctime)

        result.insert(0, filename_mtime)
        if filename_ctime.date() != filename_mtime.date():
            result.insert(0, filename_ctime)

        if not result:
            print(f"Error: no date has been found in {path.name}.",
                  file=sys.stderr)
            sys.exit(1)
        elif len(result) > 1:
            print("-" * (len(path.name) + 4))
            print(Fore.YELLOW | path.name)
            print("-" * (len(path.name) + 4))
            print(output)
            print()
            for index, item_datetime in enumerate(result):
                print(f"    {index}:", Fore.YELLOW | item_datetime.date())
            print()
            date = result[int(input("Choose date: "))]
        else:
            date = result[0]

        new_path = \
            path.parent.joinpath(date.strftime('%Y%m%d') + "-" + path.name)
        while True:
            try:
                answer = input("Rename '" +
                               (Fore.YELLOW | path)
                               + "' to '" +
                               (Fore.YELLOW | new_path) +
                               "'? [y,n] ")
            except KeyboardInterrupt:
                pass

            if answer not in ["y", "n"]:
                continue
            elif answer == "y":
                move(path, new_path)
                break


if __name__ == "__main__":
    command_line_interface()
