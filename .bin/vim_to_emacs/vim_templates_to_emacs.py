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
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED “AS IS”, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

"""Convert Vim templates to emacs."""

import hashlib
import os
from pathlib import Path
from typing import Set

DEST_HOME = Path(os.environ.get("DEST_HOME", "~/")).expanduser()

VIM_TEMPLATES_PATH = Path("~/.vim/templates").expanduser()
EMACS_TEMPLATES_PATH = \
    DEST_HOME / ".emacs-data/etc/file-templates-auto/"
EMACS_ELISP_FILE = EMACS_TEMPLATES_PATH.joinpath("main.el")


def md5sum_file(filename: os.PathLike) -> str:
    md5 = hashlib.md5()
    with open(filename, "rb") as fhandler:
        for chunk in iter(lambda: fhandler.read(1024), b""):
            md5.update(chunk)
    return md5.hexdigest()


def write_if_changed(file_name: os.PathLike, content: bytes) -> bool:
    # Check MD5
    content_md5sum = hashlib.md5(content).hexdigest()
    try:
        if content_md5sum == md5sum_file(file_name):
            return False
    except FileNotFoundError:
        pass

    # Convert to Emacs
    with open(file_name, "wb") as fhandler:
        fhandler.write(content)

    return True


def convert_vim_template_to_emacs(vim_template: os.PathLike,
                                  emacs_template: os.PathLike):
    # Load Vim template
    with open(vim_template, "rb") as fhandler:
        content = fhandler.read()
        content = content.replace(b"%HERE%", b"${1:}")

    if write_if_changed(emacs_template, content):
        print(f"[CONVERT] {vim_template} -> {emacs_template}")
    else:
        print(f"[IGNORE] {emacs_template}")


def elisp_escape_regex(string: str):
    return string.replace("\\", "\\\\").replace(".", "\\\\.")


def elisp_escape_string(string: str):
    return string.replace("\\", "\\\\")


def convert_extensions_to_elisp(templates_filenames: Set[str]):
    result = ""
    result += ";;; main.el --- Templates -*- lexical-binding: t; -*-\n"
    result += "\n;;; Code:\n"
    result += "(setq\n"
    result += "  auto-insert 'other\n"
    result += "  auto-insert-alist\n"
    result += "    '(\n"
    for template_filename in sorted(templates_filenames):
        ext = template_filename.replace("=template=", "")
        result += '      (("' + \
            elisp_escape_regex(ext) + \
            "\\\\'\" . \"Template Name\") . [\"" + \
            elisp_escape_string(template_filename) + \
            "\" my/autoinsert-yas-expand])" + "\n"

    result += "      ))\n"
    result += "\n;; Local variables:\n"
    result += ";; byte-compile-warnings: (not obsolete free-vars)\n"
    result += ";; End:\n"
    result += "\n;;; main.el ends here\n"
    return result


def convert_vim_templates_to_emacs():
    os.makedirs(EMACS_TEMPLATES_PATH, exist_ok=True)
    templates_filenames = set()
    for template_filename in os.listdir(VIM_TEMPLATES_PATH):
        source_file = VIM_TEMPLATES_PATH.joinpath(template_filename)
        dest_file = EMACS_TEMPLATES_PATH.joinpath(template_filename)
        convert_vim_template_to_emacs(source_file, dest_file)

        templates_filenames.add(template_filename)

    # Create elisp
    elisp_code = \
        convert_extensions_to_elisp(templates_filenames).encode("ascii")
    if write_if_changed(EMACS_ELISP_FILE, elisp_code):
        print("[WRITE ELISP CODE]", EMACS_ELISP_FILE)
    else:
        print("[IGNORE]", EMACS_ELISP_FILE)


def main():
    """The command-line interface."""
    convert_vim_templates_to_emacs()


if __name__ == "__main__":
    main()
