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
"""SSH and SCP wrapper."""


import logging
import shlex
import sys

from cmdwrapper import CmdWrapper

assert sys.version_info >= (3, 3), "The Python version need to be >= 3.3"


class SSH(object):
    """A wrapper around ssh / scp with methods similar to subprocess module."""

    def __init__(self, host, shell=['/bin/sh', '-c'],
                 ssh_wrapper=CmdWrapper('ssh'),
                 scp_wrapper=CmdWrapper('scp')):
        """Init the SSH class.

        Params:
            :host: the host name.
            :shell: use a specific shell to execute commands or None to let
            ssh choose the default shell.

        """
        assert isinstance(host, str)
        assert isinstance(shell, list)
        assert isinstance(ssh_wrapper, CmdWrapper)
        assert isinstance(scp_wrapper, CmdWrapper)
        self.host = host
        self.ssh = ssh_wrapper
        self.scp = scp_wrapper
        self.shell = shell

    def __call__(self, cmd):
        """Run an SSH command."""
        args = [self.host] + self._gen_args(cmd)
        return self.ssh(*args)

    def upload(self, *args):
        """Run scp from local files --> remote.

        To upload the remote files /etc/fstab and /etc/cryptsetup to the local
        directory /etc/:
        >>> self.upload('/etc/fstab', '/etc/cryptsetup', '/etc/')

        """
        assert len(args) >= 2, 'You need at least 2 arguments'
        first_args = list(args[0:len(args) - 1])
        last_arg = [self.host + ':' + shlex.quote(args[-1])]
        scp_args = first_args + last_arg
        self.scp(*scp_args).wait()

    def download(self, *args):
        """Run scp from remote --> local.

        To download the remote file /etc/fstab to the local directory
        /home/user/:
        >>> self.download('/etc/fstab', '/home/user')

        """
        assert len(args) >= 2, 'You need at least 2 arguments'
        first_args = []
        for arg in args[0:len(args) - 1]:
            first_args.append(self.host + ':' + shlex.quote(arg))
        last_arg = [args[-1]]
        scp_args = first_args + last_arg
        self.scp(*scp_args).wait()

    def _gen_args(self, cmd):
        """Build a list of arguments from a string/list or a tuple."""

        if isinstance(cmd, str):
            cmd = shlex.split(cmd)
        elif isinstance(cmd, tuple):
            cmd = list(cmd)
        elif isinstance(cmd, list):
            pass
        else:
            raise ValueError('cmd needs to be a list or a tuple')

        # if self.shell is not None, the command will not be passed
        # directly to ssh. It will be inside a "/bin/sh -c command"
        if self.shell is not None:
            args = []
            for item in cmd:
                args += [shlex.quote(item)]
            cmd = self.shell + [' '.join(args)]

        # because ssh needs the quotes for arguments with a space
        return [shlex.quote(item) for item in cmd]

# vim:ai:et:sw=4:ts=4:sts=4:tw=78:fenc=utf-8
