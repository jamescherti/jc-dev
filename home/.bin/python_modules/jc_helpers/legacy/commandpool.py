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
"""Start system commands."""

from concurrent.futures import ThreadPoolExecutor, as_completed

from command import Command

TIMEOUT_KILL = 10


class CommandPool(object):
    """Start a list of programs in the background."""

    def __init__(self, max_workers, timeout=None,
                 timeout_kill=TIMEOUT_KILL):
        """Init the command list.

        Params:
            max_workers: concurrent commands
        """
        self.timeout = timeout
        self.timeout_kill = timeout_kill
        self.max_workers = max_workers
        self.commands = []

    def append(self, command):
        """Add a command."""
        self.commands.append(command)

    def run(self):
        """Run the commands you added with self.append().

        This function will yield the executed commands.
        Example:
            cpool = CommandPool(5, timeout=20)
            for command in cpool.run():
                print(command)

        """
        with ThreadPoolExecutor(self.max_workers) as executor:
            future_commands = {executor.submit(self.worker, command):
                               command for command in self.commands}

            for future in as_completed(future_commands):
                yield future.result()

    def worker(self, command):
        """Start the system command 'command'.

        Return the Command() pointer.
        """
        command = Command(command, timeout=self.timeout,
                          timeout_kill=self.timeout_kill,
                          thread=False)
        command.wait()
        return command


def main():
    """main test."""
    cpool = CommandPool(1, timeout=2)
    for _ in range(10):
        cpool.append('find /')

    for command in cpool.run():
        print(command)

    print("FINISH!")


if __name__ == '__main__':
    main()

# vim:ai:et:sw=4:ts=4:sts=4:tw=78:fenc=utf-8
