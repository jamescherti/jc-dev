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

import shlex
import subprocess
from typing import List, Union


def run_command(command: Union[str, List[str]],
                **kwargs) -> List[str]:
    """Execute a command and return stdout as a list of strings."""
    if isinstance(command, str):
        command = shlex.split(command)
    result = subprocess.run(command, stdout=subprocess.PIPE,
                            check=True, text=True, **kwargs)
    return result.stdout.splitlines()  # type: ignore
