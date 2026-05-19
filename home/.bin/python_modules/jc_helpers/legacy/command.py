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

import shlex
import time
from subprocess import PIPE, Popen, TimeoutExpired


class Command(object):
    """Run a command.

    Features:
        - string or list command (list like subprocess.Popen)
        - timeout support: it stops + kills the process after a certain time.

    """

    class ProcessKilled(Exception):
        """Raised when the process is killed."""

        pass

    def __init__(self, cmd, timeout=None, timeout_kill=None):
        """Run the command 'cmd'.

        Params:
            cmd: the command in a list: ['find', '/'] You can also use
            strings: "find /".

            timeout: in seconds (or None, to disable the timeout) If timeout
            is specified, the command will be terminated after 'tiemout'
            seconds. If, after the termination (SIGTERM) the command doesn't
            want to terminate, it will be killed after 'timeout_kill' seconds.

            timeout_kill: when the process is terminated (SIGTERM), the class
            will wait 'timeout_kill' seconds to SIGKILL the process.

        """
        assert isinstance(cmd, (str, dict))

        if isinstance(cmd, str):
            cmd = shlex.split(cmd)

        self.cmd = cmd
        self.timeout = timeout
        self.timeout_kill = timeout_kill

        # final results
        self.stdout, self.stderr = ('', '')

        # init self._proc
        self._proc = Popen(self.cmd, stdout=PIPE, stderr=PIPE)

    def _run_command(self):
        """Start a command and stop it after self.timeout seconds."""
        try:
            stdout, stderr = self._proc.communicate(timeout=self.timeout)
        except TimeoutExpired:
            self.stdout = ''
            self.stderr = ''
            self.terminate(wait=False)
        else:
            self.stdout = stdout.decode('utf-8')
            self.stderr = stderr.decode('utf-8')

    def proc_wait(self, timeout):
        """It lets self._proc run for 'timeout' seconds.

        Return True if the time was up (and the process terminated).
        Or False if the program was stopped without a timeout (normal shutdown)
        """
        # the command is not yet started
        if timeout is None or self._proc is None:
            return False

        # already stopped
        if self._proc.poll() is not None:
            return False

        # wait until the process stops by itself or the time is up
        is_timeout = True
        poll_seconds = .1
        deadline = time.time() + timeout
        while time.time() < deadline:
            if self._proc.poll() is None:
                is_timeout = False    # it was stopped by itself
                break
            else:
                time.sleep(poll_seconds)

        return is_timeout

    def wait(self):
        """Wait until the process terminates.

        It returns the the return code of the process (or None if no
        command has been started with self.run()).

        """
        if self._proc is None:
            return None  # the command is not yet started

        self._proc.wait()

        return self._proc.returncode

    def terminate(self, wait=True, terminate_now=True):
        """Terminate the process.

        If wait=True: wait until the process is stopped.

        Param 'terminate_now': True will terminate the process right now.
        False will only set self.terminate_now, to send a message to the
        other methods (like proc_wait) to terminate the process in the
        background.

        Return: the return code of the proess is returned (or None if no
        command is started).
        """
        if not self.running():
            return None

        # SIGTERM
        self._proc.terminate()

        if self._proc_wait(self.timeout_kill):
            # try:
            # kill if the time is up
            if self.running():
                self._proc.kill()
                raise Command.ProcessKilled
            # except ProcessLookupError:
            #     pass

        # wait and return the returncode
        if wait:
            return self.wait()

        return self._proc.returncode

    def output(self):
        """Return the output: (stdout, stderr)."""
        return (self.stdout, self.stderr)

    def running(self):
        """Return True if the process is running."""
        if self._proc is None or self._proc.poll() is not None:
            return False
        else:
            return True


def main():
    """main test."""
    Command('ls /')
    print("FINISH!")


if __name__ == '__main__':
    main()

# vim:ai:et:sw=4:ts=4:sts=4:tw=78:fenc=utf-8
