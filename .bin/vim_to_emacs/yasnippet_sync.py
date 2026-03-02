#!/usr/bin/env python
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Distributed under terms of the MIT license.
#
# Copyright (C) 2004-2026 James Cherti
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in
# all copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

"""Synchronize Yasnippet snippets."""

import os
import re
import sys
from pathlib import Path
from typing import Dict, List, Union


def yasnippet_get_vars_from_comments(
        source_code_content: str,
        comment_pattern: str = r'[^\w\s]+') -> Dict[str, list]:
    """Extract variables/values from source code comments.

    Source code example:
        #!/usr/bin/env python
        # This is a simple comment.
        print("Hello world")
        #
        # myvar: value 1
        # myvar: value 2
        # myvar: value 3
        # AnotherVar: value 1

    Here is how to extract the variables and their values from the
    source code above:
    >>> get_variables_from_comments(source_code_content)
    {'AnotherVar': ['value 1'], 'myvar': ['value 1', 'value 2', 'value 3']}

    """
    source_code_lines = source_code_content.splitlines()

    result: Dict[str, list] = {}
    for line in source_code_lines:
        re_str = (r'^\s*' +
                  comment_pattern +
                  r'\s*([\w\d]+)\s*:\s*(.*)\s*$')
        match_result = re.search(re_str, line)
        if match_result:
            var_name = match_result.group(1)
            var_value = match_result.group(2)

            if var_name not in result:
                result[var_name] = []

            result[var_name].append(var_value)

    return result


def yasnippet_get_key(snippet_file: os.PathLike):
    with open(snippet_file, "r", encoding="utf-8") as fhandler:
        variables = yasnippet_get_vars_from_comments(
            fhandler.read(),
            comment_pattern="#",
        )

        if "key" in variables:
            result = os.path.basename(variables["key"][0])
            if result == ".":
                result = ""
            return result

    return Path(snippet_file).name


def yasnippet_sync_snippets(
        dir1: os.PathLike,
        dir2: os.PathLike,
        ignore_filenames: List[str],
        whitelist_modes=None,
        ignore_comment_vars: Union[None, List[str]] = None,
        ignore_dest_paths: Union[None, List[os.PathLike]] = None):
    """Rsync snippets from 'dir1' to 'dir2'.

    Make sure that the filenames of the snippet that are in dir2 correspond to
    the the value of the 'key' variable that is in each snippet.

    """
    if ignore_dest_paths:
        ignore_dest_paths = [Path(item) for item in ignore_dest_paths]

    # Make sure the arguments are existing directories
    dir1 = Path(dir1)
    dir2 = Path(dir2)
    for cur_dir in (dir1, dir2):
        if not cur_dir.is_dir():
            print(
                f"Error: no such file or directory: {cur_dir}",
                file=sys.stderr)
            sys.exit(1)

    # Find emacs modes
    for emacs_mode_dir in dir1.glob("*"):
        if not emacs_mode_dir.is_dir():
            continue

        # emacs_mode_name = emacs_mode_dir.name

        for emacs_snippet_file in emacs_mode_dir.glob("**/*"):
            if not emacs_snippet_file.is_file():
                continue

            if whitelist_modes and emacs_snippet_file.name in whitelist_modes:
                continue

            if emacs_snippet_file.name in ignore_filenames:
                continue

            emacs_snippet_key = yasnippet_get_key(emacs_snippet_file)
            if not emacs_snippet_key:
                continue

            sub_dir = emacs_snippet_file.parent.relative_to(dir1)
            emacs_snippet_dest_dir = dir2.joinpath(sub_dir)

            # emacs_snippet_dest_dir = dir2.joinpath(emacs_mode_name)
            emacs_snippet_dest_file = \
                emacs_snippet_dest_dir.joinpath(emacs_snippet_key)

            if ignore_dest_paths and \
                    emacs_snippet_dest_file in ignore_dest_paths:
                # print("[IGNORE]", emacs_snippet_dest_file)
                continue

            if emacs_snippet_dest_file.exists():
                # print("[IGNORE]", emacs_snippet_dest_file)
                continue

            print("[COPY]", emacs_snippet_file, "->",
                  emacs_snippet_dest_file)

            # Read and filter the content
            content = ""
            with open(emacs_snippet_file, "r", encoding="utf-8") as fhandler:
                for line in fhandler.readlines():
                    add_line = True
                    if ignore_comment_vars:
                        for comment_var in ignore_comment_vars:
                            if re.search(r"^\s*#\s*" +
                                         re.escape(comment_var) + r"\\s*:",
                                         line):
                                print("IGNORE:", line)
                                add_line = False
                                break

                            if "env-var" in line:
                                print("Error: env-var was found.",
                                      file=sys.stderr)
                                sys.exit(1)

                    if add_line:
                        content += line

            os.makedirs(emacs_snippet_dest_dir, exist_ok=True)
            with open(emacs_snippet_dest_file, "w",
                      encoding="utf-8") as fhandler:
                fhandler.write(content)
