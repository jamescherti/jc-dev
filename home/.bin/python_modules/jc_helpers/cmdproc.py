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
"""
Run a process.

Python-CmdWrapper is a set of object oriented classes that can help you wrap
any Linux command and use it as a Python 3 method.

## Example
```
>>> from CmdWrapper import CmdWrapper

>>> find = CmdWrapper('find')

>>> result = find('/etc', '-maxdepth', '1', '-name', 'e*')
>>> print('stdout:', result.stdout)
/etc
/etc/ethertypes
/etc/environment

>>> print('stdout lines:', result.stdout.lines)
['/etc', '/etc/ethertypes', '/etc/environment']

>>> print('stdout firstline:', result.stdout.firstline)
/etc

>>> print('exit-code:', result.returncode)
0

```

"""

import os
import shlex
import subprocess
from typing import Dict, List, Union

PIPE = subprocess.PIPE
DEVNULL = subprocess.DEVNULL
STDOUT = subprocess.STDOUT
TOTO = 2


class CmdOutput:
    """The output of a command (stdout or stderr)."""

    def __init__(self, output):
        """Store the output internally."""
        self._output = None
        self.output = output

    @property
    def lines(self):
        """Return the output as a list (each item is a line)."""
        return self.output.splitlines()

    @property
    def firstline(self):
        """Return the first line of the output."""
        if self.output != '':
            return self.lines[0]

        return ''

    def __str__(self):
        """Return the output."""
        return self.output

    def __iter__(self):
        """Iter through stdout."""
        for line in self.lines:
            yield line

    @property
    def output(self):
        """Return the output's content (string)."""
        return self._output

    @output.setter
    def output(self, output):
        """Return an unified version of the output."""
        if output is None:
            output = ''
        else:
            assert isinstance(output, (bytes, str))

        if isinstance(output, bytes):
            output = output.decode('utf-8', errors='ignore')

        self._output = output


class CmdProcError(Exception):
    """Exception raised when a process fails (returncode != 0)."""

    def __init__(self, error_msg, cmd_proc=None):
        """Store the cmd_proc (which contains stdout, stderr, returncode).

        error_msg: the Python exception's error message (string)
        cmd_proc: the cmd_proc instance

        """
        assert isinstance(error_msg, str)
        if cmd_proc is not None:
            assert isinstance(cmd_proc, CmdProc)

        self._error_msg = error_msg
        self._cmd_proc = cmd_proc
        super().__init__(self._error_msg)


class CmdProc:
    """Low level process management (run process, wait until completed...)."""

    def __init__(self, cmd: Union[List[str], str, None],
                 cwd: Union[os.PathLike, None] = None,
                 env: Union[None, Dict[str, str]] = None,
                 stdout: int = PIPE,
                 stderr: int = PIPE,
                 input_data: Union[bytes, None] = None,
                 timeout: Union[int, None] = None):
        """
        Init the process.

        :cmd: the command line arguments. Example: ['ls', '/'] or 'ls /'

        :cwd: the directory where the command is going to be executed
              (None = current directory)

        :env: rewrite the environment variables (key/value)

        :stdout: could contain PIPE, DEVNULL and STDOUT. Behaves exactly like
                 Popen's argument stdout.

        :stderr: could contain PIPE, DEVNULL and STDOUT. Behaves exactly like
                 Popen's argument stderr.

        :input: the input content. Equivalent to 'input' in Popen.communicate.

        :timeout: SIGTERM will be sent to the process after 'timeout' seconds.
        To disable this feature: 'timeout=None'.

        """
        self._logger = self._logger.getLogger(self.__class__.__name__)
        self._cmd_list, self._cmd_str = self._cmd_split_types(cmd)

        # used by process opener
        self._opts = {'cwd': cwd,
                      'env': env,
                      'stdout': stdout,
                      'stderr': stderr,
                      'input': input_data,
                      'timeout': timeout}

        self._proc = None

        self.returncode = None
        self.stdout = b''
        self.stderr = b''

    def run(self):
        """Run the command."""
        # Avoid running the process 2 times
        if self._proc is not None:
            return False

        if self._opts['cwd'] or self._opts['timeout']:
            self._logger.debug(
                '[RUN-OPTIONS] CWD:%s TIMEOUT:%s', str(self._opts['cwd']),
                str(self._opts['timeout']))

        self._logger.debug('[RUN-CMD] %s', self._cmd_str)

        # manage the stdin
        stdin = None
        if self._opts['input']:
            stdin = PIPE

        # Run the process
        self._proc = subprocess.Popen(args=self._cmd_list,
                                      stdout=self._opts['stdout'],
                                      stderr=self._opts['stderr'],
                                      stdin=stdin,
                                      cwd=self._opts['cwd'],
                                      env=self._opts['env'])

        return True

    def wait(self):
        """Wait until the process is terminated."""
        if self._proc is None:
            self.run()

        if self._proc.poll() is not None:
            # The process is stopped
            return False

        self.returncode = None
        self.stdout = b''
        self.stderr = b''
        try:
            self.stdout, self.stderr = \
                self._proc.communicate(input=self._opts['input'],
                                       timeout=self._opts['timeout'])

            if self.stdout is None:
                self.stdout = b''

            if self.stderr is None:
                self.stderr = b''

            self.returncode = self._proc.returncode

            if self.returncode != 0:
                raise CmdProcError('exit-code {} return by: {}'
                                   .format(self.returncode,
                                           self._cmd_str), self)
        except (subprocess.CalledProcessError, CmdProcError) as err:
            raise CmdProcError(self._cmd_error_msg(str(err)), self)

        return True

    def _cmd_error_msg(self, err_msg):
        """Return a string you can use for the command's exception."""
        output = self.stdout.rstrip()
        output += (b'' if output == b'' else os.linesep.encode())
        output += self.stderr
        error_msg = '{}\n\nCOMMAND: {}\n\nOUTPUT: {}\n' \
                    .format(err_msg,
                            self._cmd_str,
                            output)
        return error_msg

    @staticmethod
    def _cmd_split_types(cmd):
        """Convert a command 'cmd' to list + str."""
        assert isinstance(cmd, (list, str))

        assert isinstance(cmd, (str, list))
        if isinstance(cmd, str):
            cmd_list = shlex.split(cmd)
            cmd_str = cmd
        elif isinstance(cmd, list):
            cmd_list = cmd

            # just for the output. Don't use cmd_str to execute.
            cmd_str = ' '.join(cmd)

        assert isinstance(cmd_list, list)
        assert isinstance(cmd_str, str)

        return cmd_list, cmd_str
