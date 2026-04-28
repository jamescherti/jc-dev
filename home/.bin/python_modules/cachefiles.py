#!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Copyright (C) 2004-2026 James Cherti
#
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
"""Cache files."""

import os
import time
import hashlib


def sha512sum(filename):
    """Return the SHA512 hash of a file."""
    sha512 = hashlib.sha512()
    with open(filename, 'rb') as fhandler:
        for chunk in iter(lambda: fhandler.read(4096), b''):
            sha512.update(chunk)
    return sha512.hexdigest()


class CacheFilesError(Exception):
    """Exception raised by the class 'CacheFiles'."""

    # pylint: disable=W0107
    pass


class CacheFiles:
    """Cache files."""

    def __init__(self, cachedir):
        """Init the class.

        :cachedir: the directory where the files will be cached.

        """
        self.cachedir = os.path.abspath(cachedir)

    def load(self, filename, mode='r'):
        """Load the file from the cache.

        :filename: path to the file.
        :return: the file's content.

        """
        cachefile_path = self._get_cachefile_path(filename)
        with open(cachefile_path, mode, encoding="utf-8") as fhandler:
            return fhandler.read()

    def save(self, filename, content, mode='w'):
        """Save 'content' (string) to the cache."""
        cachefile = self._get_cachefile_path(filename)
        os.makedirs(os.path.dirname(cachefile), exist_ok=True)

        with open(cachefile, mode, encoding="utf-8") as fhandler:
            fhandler.write(content)

        # Update the file's time (update: atime, mtime)
        if os.path.isfile(cachefile):
            atime = time.time()
            mtime = atime
            os.utime(cachefile, (atime, mtime))

    def _get_cachefile_path(self, filename):
        """Return a path to the cached version of 'filename'."""
        filename = os.path.abspath(filename)

        # Are we are inside the hierarchy self.cachedir?
        cachedir_slash = self.cachedir + os.path.sep
        if filename[0:len(cachedir_slash)] == cachedir_slash:
            raise CacheFilesError("You cannot cache the files that "
                                  "are inside the directory "
                                  f"'{cachedir_slash}'")

        sha512 = sha512sum(filename)
        cache_path = os.path.join(self.cachedir, sha512)

        return cache_path
