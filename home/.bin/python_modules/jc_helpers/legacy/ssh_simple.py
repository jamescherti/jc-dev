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
"""Generate ssh, scp, and rsync command-line arguments."""


class SimpleSshGen:
    """Generate ssh, scp, and rsync command-line arguments."""

    def __init__(self, host: str):
        self.host = host

    def upload_args(self, local_paths: list, remote_path: str):
        """Return the scp/rsync command-line arguments to upload files."""
        remote_uri = ["{}:{}".format(self.host, remote_path)]
        return local_paths + remote_uri

    def download_args(self, remote_paths: list, local_path: str):
        """Return the scp/rsync command-line arguments to download files."""
        new_remote_paths = ["{}:{}".format(self.host, item)
                            for item in remote_paths]
        return new_remote_paths + [local_path]

    def ssh_args(self, cmd: list):
        """Return the ssh command-line arguments to run 'cmd' remotely."""
        return [self.host] + cmd
