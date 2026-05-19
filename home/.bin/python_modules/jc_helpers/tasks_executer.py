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
"""This module defines a set of classes for task execution.

Features:
- Method for searching files within directories and their parent directories.
- The Variables class is a utility class for managing variables.
- The JinjaRenderer class is used for rendering templates using Jinja2.
- The Tasks class is used for managing tasks and executing them iteratively or
  concurrently using threads.
- And many others.

"""

from concurrent.futures import ThreadPoolExecutor, as_completed
from pprint import pformat
from typing import Any, Callable, Generator


class Tasks(list):
    def __init__(self, data: Any = None):
        super().__init__()
        self.data = data

    def execute(self, method: Callable) -> Generator:
        """Execute tasks using a specified method."""
        for task in self:
            yield method(self, task)

    def execute_threaded(self, method: Callable, **kwargs) -> Generator:
        """Execute tasks using a specified method with multi-threading.

        This method is similar to ThreadPoolExecutor with the following
        parameters:
            max_workers: The maximum number of threads that can be utilized for
            executing the provided tasks.

            thread_name_prefix: An optional prefix to name the threads.

            initializer: A callable function used to initialize worker threads.

            initargs: A tuple of arguments to pass to the initializer.
        """
        with ThreadPoolExecutor(**kwargs) as executor:
            futures = []
            for task in self:
                futures.append(executor.submit(method, self, task))

            for future in as_completed(futures):
                yield future.result()

    def __repr__(self):
        """Return a string representation of the tasks."""
        return pformat(self)
