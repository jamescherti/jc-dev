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
"""Convert supported file formats to text (using OCR).

Supported files:
- images
- PDFs
- Text files

Linux Dependencies (commands):
- tesseract
- pdftotext (poppler)
- pdfimages (poppler)

To install the commands above on Ubuntu / Debian:
# apt-get install tesseract-ocr-eng poppler-utils

To install them on Arch Linux:
# pacman -S tesseract tesseract-data-eng poppler

"""

import mimetypes
import os
import shutil
import subprocess
import sys
import tempfile
from pathlib import Path
from typing import Union
import argparse


from cachefiles import CacheFiles

SUPPORTED_IMAGES = ['image/png', 'image/jpeg']


class ReadTextError(Exception):
    """Exception raised by the class ReadText()."""


class ReadText:
    """Convert any supported file to text (images, PDFs and text files).

    1. Images: Tesseract OCR is used to convert them to text.
    1. PDF: the text is extracted and the images are converted to text with
       Tesseract OCR.
    2. Texts: they are loaded without any transformation.

    Images and PDFs are cached. They will be loaded faster the second time you
    load them if cache=True.

    """

    class_init_done = False    # static variable

    def __init__(self, ocr: bool, cache: bool, tesseract_lang):
        self.ocr = ocr
        self.tesseract_lang = tesseract_lang
        self.cache: Union[None, CacheFiles] = None

        if not ReadText.class_init_done:
            mimetypes.init()
            list_commands = ["pdftotext"]
            if self.ocr:
                list_commands += ["tesseract"]
                list_commands += ["pdfimages"]

            for command in list_commands:
                if not shutil.which(command):
                    error = f"Error: the command '{command}' was not found"
                    raise ReadTextError(error)
            ReadText.class_init_done = True

        if cache:
            self.cache = CacheFiles(os.path.join(os.path.expanduser("~"),
                                                 '.cache',
                                                 'readtext_py',
                                                 'ocr_cache'))

    def load(self, filename: Union[str, Path]):
        """Return the text version of a file ."""
        filename = Path(filename)
        mime, _ = mimetypes.guess_type(filename)
        if not mime:
            mime = 'unknown/unknown'

        if mime == 'application/pdf':
            content = self.__load_pdf(filename)
        elif mime.startswith("application/epub"):
            content = self.__load_epub(filename)
        elif mime in SUPPORTED_IMAGES:
            if self.cache:
                try:
                    content = self.cache.load(filename)
                    return content
                except FileNotFoundError:
                    content = self.__load_image(filename)
        elif mime.startswith('text/'):
            with open(filename, 'r', encoding="utf-8") as fhandler:
                content = fhandler.read()
                return content  # no cache
        else:
            raise ReadTextError(f"The mime type '{mime}' of the "
                                f"file '{filename}' is not supported")

        if self.cache:
            self.cache.save(filename, content)

        # Reduce multiple empty lines into one
        # content = re.sub(r"\n\n+", r"\n\n", content)

        return content

    def __load_epub(self, filename: Union[str, Path]):
        """Read epub files."""
        cmd = ["pandoc", "-f", "epub", "-t", "plain", str(filename)]
        result = subprocess. \
            check_output(cmd, stderr=subprocess.DEVNULL).decode()

        return result

    def __load_image(self, filename: Union[str, Path]):
        """Use OCR to convert an image to text."""
        cmd = ["tesseract", str(filename), "stdout", "-l", self.tesseract_lang]
        return subprocess.check_output(cmd, stderr=subprocess.DEVNULL).decode()

    def __load_pdf(self, filename: Union[str, Path]):
        """Read text from a PDF."""
        filename = Path(filename)
        # Extract the text
        cmd = ["pdftotext", "-nopgbrk", "-layout", str(filename), "-"]
        # cmd = ['pdf2txt.py', filename]
        result = subprocess. \
            check_output(cmd, stderr=subprocess.DEVNULL).decode()

        if not self.ocr:
            return result

        # Create a temporary dir
        tmpdir = tempfile.mkdtemp(prefix="readtext", suffix="loadpdf")
        try:
            # Extract the images
            cmd = ['pdfimages', '-png', str(filename),
                   os.path.join(tmpdir, 'image')]
            subprocess.check_call(cmd,
                                  stdout=subprocess.DEVNULL,
                                  stderr=subprocess.DEVNULL)

            for image_filename in os.listdir(tmpdir):
                image_filename = os.path.join(tmpdir, image_filename)

                if self.cache:
                    try:
                        cache_content = self.cache.load(image_filename)
                        result += cache_content + "\n"
                        continue
                    except FileNotFoundError:
                        # cached version not found
                        pass

                mime, _ = mimetypes.guess_type(image_filename)
                cache_content = ""
                if mime in SUPPORTED_IMAGES:
                    cache_content = self.__load_image(image_filename)
                    if cache_content:
                        result += cache_content + "\n"
                        if self.cache:
                            self.cache.save(str(filename), cache_content)
        finally:
            shutil.rmtree(tmpdir)

        return result


def parse_args():
    """Parse the command-line arguments."""
    usage = "%(prog)s [--option] [args]"
    parser = argparse.ArgumentParser(description=__doc__.splitlines()[0],
                                     usage=usage)
    parser.add_argument("args", metavar="N", type=str, nargs="+",
                        help="Files (PDF, text...)")
    parser.add_argument(
        "-o",
        "--ocr",
        default=False,
        action="store_true",
        required=False,
        help="Enable OCR.",
    )
    parser.add_argument(
        "-c",
        "--cache",
        default=False,
        action="store_true",
        required=False,
        help="Enable the cache.",
    )
    return parser.parse_args()


def command_line_interface():
    """The command line interface of readtext."""
    # Optional: setproctitle
    try:
        # pylint: disable=import-outside-toplevel
        from setproctitle import setproctitle
        setproctitle(Path(sys.argv[0]).name)  # type: ignore
    except ImportError:
        # Optional dependency 'setproctitle' is not installed.
        pass

    args = parse_args()
    readtext = ReadText(ocr=args.ocr, cache=args.cache, tesseract_lang="eng")

    # valid commands: grep, cat...
    exit_code = 0
    for filename in sys.argv[1:]:
        try:
            text = readtext.load(filename)
        except ReadTextError as err:
            # pylint: disable=consider-using-f-string
            print("[WARNING] cannot read text from {} ({})"
                  .format(filename, str(err)),
                  file=sys.stderr)
            text = ""
            exit_code = 1
            continue

        print(text)

    sys.exit(exit_code)


if __name__ == "__main__":
    command_line_interface()
