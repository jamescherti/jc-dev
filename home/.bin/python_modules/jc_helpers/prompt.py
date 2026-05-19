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
"""Use readline to complete using a list."""


import readline
from typing import Any, List, Union


class ReadlineCompleter:
    """A readline completer."""

    def __init__(self, options: List[str]):
        """Store the options = ['word1', 'word2']."""
        readline.set_completer_delims('')
        self.options = options
        self.matches: List[str] = []

    def complete(self, _, state):
        """Complete a readline sentence."""
        if state == 0:
            origline = readline.get_line_buffer()
            begin = readline.get_begidx()
            end = readline.get_endidx()
            being_completed = origline[begin:end]
            words = origline.split()

            if not words:
                self.matches = self.options[:]
            else:
                try:
                    if begin == 0:
                        matches = self.options[:]  # First word
                    else:
                        first = words[0]  # Later word
                        matches = self.options[first]

                    if being_completed:
                        # Match options with portion of input
                        # being completed
                        self.matches = [w for w in matches
                                        if w.startswith(being_completed)]
                    else:
                        # Matching empty string so use all candidates
                        self.matches = matches
                except (KeyError, IndexError):
                    self.matches = []

        try:
            return self.matches[state]
        except IndexError:
            return None


def prompt_text(prompt: Any,
                list_options: Union[None, List[str]] = None):
    """Read a string from standard input and complete against 'list_options'.

    The trailing newline is stripped. The prompt string is printed to
    standard output without a trailing newline before reading input.

    If the user hits EOF (*nix: Ctrl-D, Windows: Ctrl-Z+Return), raise
    EOFError. On *nix systems, readline is used if available.

    """
    readline.parse_and_bind('tab: complete')
    if list_options is None:
        list_options = []

    save_completer = readline.get_completer()
    try:
        readline.set_completer(
            ReadlineCompleter(list_options).complete
        )
        return input(prompt)
    finally:
        readline.set_completer(save_completer)
