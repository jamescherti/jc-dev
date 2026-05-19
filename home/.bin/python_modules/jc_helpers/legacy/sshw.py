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
#
# TODO:
# - run multiple test -f, not only one
#   idea: isfile for multiple files on SSH
#   files = " -a ".join(files)
#   test_command = "test ! {}".format(files)
#   print(test_command)
#   return
#
"""SSH wrapper for Python."""


import logging
import os
import platform
import shlex
import sys
from fcntl import F_GETFL, F_SETFL, fcntl
from os import O_NONBLOCK
from pprint import pformat
from subprocess import DEVNULL, PIPE, Popen


class SSHError(Exception):
    """Error with SSH."""

    pass


class SSHOutput(bytes):
    """A helper to parse the stdout/stderr output.

    >>> SSHOutput(b'Line1')
    b'Line1'

    """

    def utf8(self):
        """Return the UTF-8 version of the output."""
        return self.decode('utf-8', 'ignore')

    def lines(self, *args, **kwargs):
        """Convert the output to UTF-8 and split with line return."""
        return self.utf8().splitlines(*args, **kwargs)

    def firstline(self):
        """Return the first line of the output."""
        lines = self.lines()
        return lines[0] if len(lines) > 0 else ''


class SSHResult(object):
    """Result returned by SSH.run()."""

    def __init__(self, cmd, returncode, stdout=b'', stderr=b''):
        """Init stdout, stderr and returncode.

        Params:
            :returncode: the exit code of the SSH command (required).
            :stdout: stdout content (bytes).
            :stderr: stderr content (bytes).

        """
        assert isinstance(cmd, str)
        assert isinstance(stdout, bytes)
        assert isinstance(stderr, bytes)
        assert isinstance(returncode, int)
        self.cmd = cmd
        self.stdout = SSHOutput(stdout)
        self.stderr = SSHOutput(stderr)
        self.returncode = returncode

    def __repr__(self):
        """Pretty print stdout/stderr/returncode."""
        return pformat({'cmd': self.cmd,
                        'stdout': self.stdout,
                        'stderr': self.stderr,
                        'returncode': str(self.returncode)})


class SSH(object):
    """SSH wrapper."""

    def __init__(self, host, ssh_opts=None):
        """Init SSH. The default working directory is '/'."""
        assert isinstance(host, str)
        self.host = host
        self._cwd = '/'

        assert isinstance(ssh_opts, (list, str)) or ssh_opts is None
        if isinstance(ssh_opts, str):
            ssh_opts = shlex.split(ssh_opts)
        self.ssh_opts = ssh_opts if ssh_opts else []

    def _local_run(self, cmd, ignore_errors=False, input=None, timeout=None,
                   **popen_kwargs):
        """Run a command. Check the docstring of SSH.run() for the params."""
        logging.debug('[SSH-RUN] %s', cmd)

        proc = Popen(cmd, **popen_kwargs)
        logging.debug('Process PID: %i', proc.pid)

        # put the file handler in non-blocking mode and allow functions
        # like proc.stdout.read() to return data if any is available
        flags = fcntl(proc.stdout, F_GETFL)
        if 'stdout' in popen_kwargs and popen_kwargs['stdout'] == PIPE:
            fcntl(proc.stdout, F_SETFL, flags | O_NONBLOCK)

        if 'stderr' in popen_kwargs and popen_kwargs['stderr'] == PIPE:
            fcntl(proc.stderr, F_SETFL, flags | O_NONBLOCK)

        stdout, stderr = proc.communicate(input=input, timeout=timeout)

        returncode = proc.returncode
        del proc

        if returncode and not ignore_errors:
            raise SSHError("The exit-code isn't 0: {}"
                           .format(self.cmdline(cmd)))
        else:
            return SSHResult(cmd=self.cmdline(cmd), returncode=returncode,
                             stdout=stdout if stdout else bytes(),
                             stderr=stderr if stderr else bytes())

    def run(self, cmd, cwd=None, ignore_errors=False, input=None,
            timeout=None, stdin=None, stdout=PIPE, stderr=PIPE,
            **popen_kwargs):
        """Run a command in the remote SSH server with subprocess.Popen() .

        Parameters:
            cmd: the command you want to run (list or string)
                     Example: ['ls', '/']
                              'ls /etc'
            cwd: run the cmd in another remote directory than self.cwd
                 (to change self.cwd, use the method self.cd() )
            timeout: proc.communicate's timeout
            ignore_errors: True to ignore a returncode that is
                           different than 0. False to raise an
                           SSHError exception (default).
            input: the content of stdin (passed to proc.communicate)
            stdin/stdout/stderr: passed to Popen(). You can use
                                 stdout=subprocess.DEVNULL for example.
            :**popen_kwargs: arguments passed to Popen(), as well as stdin,
                             stdout, stderr above.

        Return
            SSHResult instance (contains stdout, stderr and returncode)

        """
        if isinstance(cmd, str):
            cmd = shlex.split(cmd)
        else:
            assert isinstance(cmd, list)

        # cwd: current working directory
        if cwd is None:
            # the current working directory
            cwd = [self.cwd]
        elif cwd == '':
            # home directory
            cwd = []
        else:
            # the value you enforce in the arguments
            cwd = [cwd]

        cmd = ['ssh'] + self.ssh_opts + [self.host] + ['cd'] + cwd + \
            ['&&'] + cmd

        return self._local_run(cmd, ignore_errors=ignore_errors,
                               input=input, timeout=timeout,
                               stdin=stdin, stdout=stdout, stderr=stderr,
                               **popen_kwargs)

    def runi(self, source_code, interpreter, **kwargs):
        """Run the interpreter and send source_code in stdin.

        >>> self.runi('print("Hello world")', '/usr/bin/env python')
        Hello world

        """
        if isinstance(source_code, str):
            source_code = source_code.encode('utf-8')

        assert isinstance(source_code, bytes)
        return self.run(cmd=interpreter, input=source_code, stdin=PIPE,
                        **kwargs)

    def python(self, source_code, version='', **kwargs):
        """Run Python in the remote server."""
        return self.runi(source_code=source_code,
                         interpreter='/usr/bin/env python' + version,
                         **kwargs)

    def bash(self, source_code, **kwargs):
        """Run source_code in bash."""
        return self.runi(source_code=source_code,
                         interpreter='/usr/bin/env bash',
                         **kwargs)

    @property
    def cwd(self):
        """Return the current working directory."""
        return self._cwd

    @property
    def pwd(self):
        """Return the current working directory."""
        return self._cwd

    def _scp(self, files, scp_opts=None, **kwargs):
        """Copy files with scp.

        :files: a list of files.
        :scp_opts: the scp arguments.
        :args: options passed to Popen.communicate().
        :kwargs: options passed to Popen.communicate().
        :return: the exit code of the scp command (0 = success).

        """
        scp_opts = [] if scp_opts is None else scp_opts
        cmd = ['scp'] + scp_opts + files
        return self._local_run(cmd, stdin=DEVNULL, stdout=DEVNULL,
                               stderr=DEVNULL, **kwargs)

    def upload(self, files, target=None, scp_opts=None, **kwargs):
        """Upload files to a destination with scp.

        :files: a list of files.
        :target: the target file or direcftory to copy data to.
                 None to upload to the current working directory self.cwd .
        :return: the exit code of scp.

        """
        assert isinstance(files, list)
        assert isinstance(target, (str, type(None)))

        if target is None:
            target = self.cwd

        return self._scp(files + ['{}:{}'.format(self.host, target)],
                         scp_opts, **kwargs)

    def download(self, remote_files, target, scp_opts=None, **kwargs):
        """Download files from the SSH server.

        :files: a list of files.
        :target: the target file or direcftory to copy data to.
        :return: the exit code of scp.

        """
        assert isinstance(remote_files, list)
        assert isinstance(target, str)

        remote_files = ['{}:{}'.format(self.host, fname)
                        for fname in remote_files]

        return self._scp(remote_files + target, scp_opts,
                         **kwargs)

    def cd(self, cwd='', check=True):
        """Change the current directory.

        The next command you will run with self.run will be started in the
        directory you specified with cd(). This function will test
        will test the directory, without modifying the global directory
        self.cwd .

        If check=False, self.cwd will be changed without any check.
        If cwd='' then cd to the home directory

        """
        self._cwd = cwd

        if check:
            try:
                cwd = self.run('pwd', cwd=cwd).stdout.line()
                self._cwd = cwd
            except SSHError:
                raise SSHError("Cannot change the directory to "
                               "'{}' on '{}'.".format(cwd, self.host))

    def ls(self, directory='.', **kwargs):
        """Get the files if a directory (kwargs is passed to self.run)."""
        result = self.run(['ls', '-1', directory], **kwargs)
        if result.returncode == 0:
            lines = []
            for line in result.stdout.lines():
                lines.append(os.path.join(directory, line))

            return lines
        else:
            return []

    def cat(self, directory, **kwargs):
        """Return the result of 'cat' in a list (line by line)."""
        result = self.run(['cat', directory], **kwargs)
        return result.stdout.lines() if result.returncode == 0 else []

    def isfile(self, directory):
        """Check if a file exists in the server."""
        return self.run(['test', '-f', directory],
                        ignore_errors=True,
                        stdout=DEVNULL, stderr=DEVNULL).returncode == 0

    def isdir(self, directory):
        """Check if a directory exists in the server."""
        return self.run(['test', '-d', directory],
                        ignore_errors=True, stdout=DEVNULL,
                        stderr=DEVNULL).returncode == 0

    def islink(self, directory):
        """Check if a link exists in the server."""
        return self.run(['test', '-L', directory],
                        ignore_errors=True, stdout=DEVNULL,
                        stderr=DEVNULL).returncode == 0

    def exists(self, directory):
        """Check if a file exists."""
        return self.run(['test', '-e', shlex.quote(directory)],
                        ignore_errors=True, stdout=DEVNULL,
                        stderr=DEVNULL).returncode == 0

    def filesize(self, filename):
        """Get the filesize.

        Returns -1 on error
        Or the filesize in bytes on success

        """
        result = self.run("du -s {}".format(shlex.quote(filename)))
        return int(result.stdout.lines()[0].split()[0])

    @staticmethod
    def cmdline(cmd):
        """Return a command built from the arguments.

        >>> cmdline("ls", "/etc", ["-l", "-d"])
        ls "/etc" -l -d

        """
        if isinstance(cmd, str):
            return cmd
        else:
            assert isinstance(cmd, list)

        result = ''
        double_dash = False
        for arg in cmd:
            is_option = True if arg.startswith('-') else False

            if arg == '--':
                double_dash = True

            if result != '':
                result += ' '

            if is_option and not double_dash:
                result += arg
            else:
                result += shlex.quote(arg)

        return result


# vim:ai:et:sw=4:ts=4:sts=4:tw=78:fenc=utf-8
