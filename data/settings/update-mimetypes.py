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
"""Update mimetypes."""

import os
import subprocess
import sys
from pathlib import Path
from shutil import which


def is_xfce() -> bool:
    """Return True if the current desktop environment is XFCE."""
    return os.environ.get("XDG_CURRENT_DESKTOP") == "XFCE"


def is_gnome() -> bool:
    """Return True if the current desktop environment is GNOME."""
    return os.environ.get("XDG_CURRENT_DESKTOP") == "GNOME"


def run(*args: str) -> None:
    """Execute a command and print it."""
    print("[RUN]", subprocess.list2cmdline(args))
    subprocess.run(args, check=True)


def add_mime(app_id: str, mime_type: str) -> None:
    """Set the default application for a given MIME type using xdg-mime."""
    print(f"[DEBUG] app_id:{app_id} mime_type:{mime_type}")
    desktop_file = f"/usr/share/applications/{app_id}.desktop"
    desktop_file2 = Path(f"~/.local/share/applications/{app_id}.desktop") \
        .expanduser()
    print(
        f"[DEBUG] desktop_file1:{desktop_file} desktop_file2:{desktop_file2}")

    if not os.path.isfile(desktop_file) and not desktop_file2.is_file():
        print(f"Error: '{desktop_file}' does not exist.", file=sys.stderr)
        sys.exit(1)

    try:
        run("xdg-mime", "default", f"{app_id}.desktop", mime_type)
    except subprocess.CalledProcessError:
        sys.exit(1)


def main() -> None:
    app_text: str = ""
    app_images: str = ""
    app_file: str = ""
    app_pdf: str = ""
    app_audio: str = ""
    app_video: str = ""
    app_web_browser = ""

    # Archives
    add_mime("org.gnome.FileRoller", "application/x-gzip")
    add_mime("org.gnome.FileRoller", "application/gzip")
    # add_mime("org.gnome.FileRoller", "application/vnd.android.package-archive")
    # add_mime("org.gnome.FileRoller", "application/vnd.ms-cab-compressed")
    # add_mime("org.gnome.FileRoller", "application/vnd.debian.binary-package")
    # add_mime("org.gnome.FileRoller", "application/vnd.rar")
    add_mime("org.gnome.FileRoller", "application/x-7z-compressed")
    add_mime("org.gnome.FileRoller", "application/x-7z-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-ace")
    # add_mime("org.gnome.FileRoller", "application/x-alz")
    # add_mime("org.gnome.FileRoller", "application/x-apple-diskimage")
    # add_mime("org.gnome.FileRoller", "application/x-ar")
    # add_mime("org.gnome.FileRoller", "application/x-archive")
    # add_mime("org.gnome.FileRoller", "application/x-arj")
    # add_mime("org.gnome.FileRoller", "application/x-brotli")
    # add_mime("org.gnome.FileRoller", "application/x-bzip-brotli-tar")
    add_mime("org.gnome.FileRoller", "application/x-bzip")
    add_mime("org.gnome.FileRoller", "application/x-bzip-compressed-tar")
    add_mime("org.gnome.FileRoller", "application/x-bzip1")
    add_mime("org.gnome.FileRoller", "application/x-bzip1-compressed-tar")
    add_mime("org.gnome.FileRoller", "application/x-bzip3")
    add_mime("org.gnome.FileRoller", "application/x-bzip3-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-cabinet")
    # add_mime("org.gnome.FileRoller", "application/x-cd-image")
    # add_mime("org.gnome.FileRoller", "application/x-compress")
    # add_mime("org.gnome.FileRoller", "application/x-compressed-tar")
    add_mime("org.gnome.FileRoller", "application/x-cpio")
    # add_mime("org.gnome.FileRoller", "application/x-chrome-extension")
    # add_mime("org.gnome.FileRoller", "application/x-deb")
    # add_mime("org.gnome.FileRoller", "application/x-ear")
    # add_mime("org.gnome.FileRoller", "application/x-ms-dos-executable")
    # add_mime("org.gnome.FileRoller", "application/x-gtar")
    # add_mime("org.gnome.FileRoller", "application/x-gzpostscript")
    # add_mime("org.gnome.FileRoller", "application/x-java-archive")
    # add_mime("org.gnome.FileRoller", "application/x-lha")
    # add_mime("org.gnome.FileRoller", "application/x-lhz")
    # add_mime("org.gnome.FileRoller", "application/x-lrzip")
    # add_mime("org.gnome.FileRoller", "application/x-lrzip-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-lz4")
    # add_mime("org.gnome.FileRoller", "application/x-lzip")
    # add_mime("org.gnome.FileRoller", "application/x-lzip-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-lzma")
    # add_mime("org.gnome.FileRoller", "application/x-lzma-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-lzop")
    # add_mime("org.gnome.FileRoller", "application/x-lz4-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-ms-wim")
    # add_mime("org.gnome.FileRoller", "application/x-rar")
    # add_mime("org.gnome.FileRoller", "application/x-rar-compressed")
    # add_mime("org.gnome.FileRoller", "application/x-rpm")
    # add_mime("org.gnome.FileRoller", "application/x-source-rpm")
    # add_mime("org.gnome.FileRoller", "application/x-rzip")
    # add_mime("org.gnome.FileRoller", "application/x-rzip-compressed-tar")
    add_mime("org.gnome.FileRoller", "application/x-tar")
    add_mime("org.gnome.FileRoller", "application/x-tarz")
    # add_mime("org.gnome.FileRoller", "application/x-tzo")
    # add_mime("org.gnome.FileRoller", "application/x-stuffit")
    # add_mime("org.gnome.FileRoller", "application/x-war")
    # add_mime("org.gnome.FileRoller", "application/x-xar")
    add_mime("org.gnome.FileRoller", "application/x-xz")
    add_mime("org.gnome.FileRoller", "application/x-xz-compressed-tar")
    add_mime("org.gnome.FileRoller", "application/x-zip")
    add_mime("org.gnome.FileRoller", "application/x-zip-compressed")
    add_mime("org.gnome.FileRoller", "application/x-zstd-compressed-tar")
    # add_mime("org.gnome.FileRoller", "application/x-zoo")
    add_mime("org.gnome.FileRoller", "application/zip")
    add_mime("org.gnome.FileRoller", "application/zstd")

    # MISC
    if which("emacsclient"):
        app_text = "emacsclient"
    elif which("gvim"):
        app_text = "gvim"
    elif which("gedit"):
        app_text = "org.gnome.gedit"
    elif which("mousepad"):
        app_text = "org.xfce.mousepad"

    if which("feh"):
        app_images = "feh"
    elif which("gthumb"):
        app_images = "org.gnome.gThumb"

    # File explorer
    if which("thunar"):
        app_file = "thunar"
    elif which("nautilus"):
        app_file = "org.gnome.Nautilus"

    if which("evince"):
        app_pdf = "org.gnome.Evince"

    # Audio and video
    app_audio = "vlc"
    app_video = "mpv"

    add_mime(app_file, "inode/directory")

    # PDF
    add_mime(app_pdf, "application/pdf")

    # TODO flatpak
    # if which("firefox-esr"):
    #     app_web_browser = "firefox-esr"
    # elif which("firefox"):
    #     app_web_browser = "firefox"

    # add_mime(app_web_browser, "application/xhtml+xml")
    # add_mime(app_web_browser, "text/html")

    for image_mime in ("image/webp", "image/gif", "image/png", "image/jpeg"):
        add_mime(app_images, image_mime)

    for audio_mime in ("audio/mpeg",
                       "audio/ogg",
                       "video/x-ms-asf",
                       "audio/flac"):
        add_mime(app_audio, audio_mime)

    for video_mime in ("audio/mp4", "video/mp4", "video/x-matroska",
                       "video/quicktime"):
        add_mime(app_video, video_mime)

    for text_mime in ("application/json", "text/plain", "text/xml"):
        add_mime(app_text, text_mime)

    if which("libreoffice"):
        add_mime("libreoffice-calc",
                 "application/vnd.oasis.opendocument.spreadsheet")
        add_mime("libreoffice-writer",
                 "application/"
                 "vnd.openxmlformats-officedocument.wordprocessingml.document")


if __name__ == "__main__":
    main()
