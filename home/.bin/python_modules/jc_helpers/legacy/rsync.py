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
"""rsync."""


import platform
import sys
from concurrent.futures import ThreadPoolExecutor, as_completed


def dir_slash(directory):
    """Add a slash in the end of a directory"""
    return directory.rstrip(path.sep) + path.sep


def rsync(src, dests, excludes, branch):
    """Rsync from source to a list of destinations"""
    assert isinstance(src, str)
    assert isinstance(dests, list)
    assert isinstance(excludes, list)

    if len(excludes) != 0:
        excludes_str = ''
        for exclude in excludes:
            excludes_str += ' ' + exclude

    def run_cmd(src, dest, rsync_eopt):
        """Run the command.

        rsync_eopt: a list of rsync excluded that need to be
        excluded but NOT deleted from the server.

        """
        executed_cmd = []
        list_cmd = []

        if len(RSYNC_DELETE) > 0:
            # delete RSYNC_DELETE from the destination
            list_cmd.append(['rsync'] + RSYNC_DELETE +
                            ['--delete', '--delete-excluded', '-aru',
                             src, dest])

        list_cmd.append(['rsync'] + RSYNC_DONT_SYNC + rsync_eopt +
                        ['--delete', '-aru', src, dest])

        for cmd in list_cmd:
            executed_cmd.append(cmd)

            proc = Popen(cmd, stdout=PIPE, stderr=PIPE)
            (stdout, stderr) = proc.communicate()
            output = stdout.decode('utf-8') + stderr.decode('utf-8')
            if proc.returncode != 0:
                return (dest, executed_cmd, False, output)

        return (dest, executed_cmd, True, output)

    # directories that are exluded
    rsync_eopt = []
    for edir in excludes:
        rsync_eopt.append('--exclude')
        rsync_eopt.append(edir)

    with ThreadPoolExecutor(max_workers=MAX_WORKERS) as executor:
        future = []
        for dest in dests:
            dest = dir_slash(dest)
            future.append(executor.submit(run_cmd, src, dest, rsync_eopt))

        success = 0
        failure = 0
        for future in as_completed(future):
            dest, list_cmd, result, output = future.result()

            for cmd in list_cmd:
                cmd_str = ' '.join(cmd)

            if result is False:
                failure += 1
            elif result is True:
                success += 1


def main():
    """The program starts here."""
    assert platform.system() == 'Linux', \
        'The operating system needs to be Linux'
    assert sys.version_info >= (3, 3), \
        "The Python version needs to be >= 3.3"

    sys.exit(0)


if __name__ == '__main__':
    main()
