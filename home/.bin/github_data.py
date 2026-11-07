#!/usr/bin/env python3
#
# Python Requirements:
#   - PyGithub
#   - PyYAML
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
"""Script to list all public, non-fork repositories and their categories."""

IGNORE_GITHUB_REPOSITORIES: set[str] = {
    "jamescherti/jc-dev",
    "jamescherti/modeline-region.el",
    "jamescherti/alpm-reboot-required",
    "jamescherti/emacs-ansible",
    "jamescherti/emacs-eat",
    "jamescherti/emacs-elisp-autofmt",
    "jamescherti/emacs-elpa",
    "jamescherti/emacs-flymake-yamllint",
    "jamescherti/emacs-nongnu-elpa",
    "jamescherti/emacs-undo-fu",
    "jamescherti/emacs-undo-fu-session",
    "jamescherti/emacs-ztree",
    "jamescherti/flymake-yamllint",
    "jamescherti/gcmh",
    "jamescherti/gnome-hidetopbar",
    "jamescherti/gnome-just-perfection",
    "jamescherti/org-mode",
    "jamescherti/paredit",
    "jamescherti/ztree",
}

CUSTOM_CATEGORIES: dict[str, str] = {
    # Shell
    "jamescherti/efasd": "shell",
    # Python
    "jamescherti/batchfetch": "python",
    "jamescherti/caffeine-aw": "python",
    "jamescherti/elispcomp": "python",
    "jamescherti/git-commitflow": "python",
    "jamescherti/git-rexec": "python",
    "jamescherti/git-smartmv": "python",
    "jamescherti/git-smartpull": "python",
    "jamescherti/pathaction": "python",
    "jamescherti/pdfcipher": "python",
    "jamescherti/ultyas": "python",
    "jamescherti/watch-xfce-xfconf": "python",
    # Shell
    "jamescherti/update-iptables": "shell",
    # Other
    "jamescherti/python-dpc3848v-wifi-modem": "legacy",
    "jamescherti/docker-oddmuse": "legacy",
    # "jamescherti/jamescherti": "misc",
    # "jamescherti/cherrybuckle": "python",
}
