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

from pathlib import Path

SECRETS_FILE = Path.home() / ".secrets" / "github.yaml.asc"

SRC_DIR = Path().home() / "src"

CUSTOM_CATEGORIES: dict[str, str] = {
    # Shell
    "jamescherti/efasd": "tools",
    "jamescherti/update-iptables": "tools",
    # Python
    "jamescherti/vim-client": "python",
    "jamescherti/ansible-cleanup": "python",
    "jamescherti/batchfetch": "python",
    "jamescherti/git-commitflow": "python",
    "jamescherti/git-rexec": "python",
    "jamescherti/git-smartmv": "python",
    "jamescherti/git-smartpull": "python",
    "jamescherti/pathaction": "python",
    "jamescherti/pdfcipher": "python",
    "jamescherti/ultyas": "python",
    "jamescherti/watch-xfce-xfconf": "python",
    # Emacs misc
    "jamescherti/pre-commit-elisp": "emacs-misc",
    "jamescherti/minimal-emacs.d": "emacs-misc",
    "jamescherti/lightemacs": "emacs-misc",
    # Shell
    "jamescherti/lightvim": "dotfiles",
    "jamescherti/bash-stdops": "dotfiles",
    # No category
    "jamescherti/jamescherti": "",
    "jamescherti/jc-dev": "dotfiles",
    "jamescherti/wip": "",
    "jamescherti/notes": "",
    # Other
    "jamescherti/alpm-reboot-required": "legacy",
    "jamescherti/archlinux-brother-ql1110nwb": "legacy",
    "jamescherti/archlinux-linux-keep-modules": "legacy",
    "jamescherti/aur-brother-ql1110nwb": "legacy",
    "jamescherti/aur-linux-keep-modules": "legacy",
    "jamescherti/docker-oddmuse": "legacy",
    "jamescherti/python-dpc3848v-wifi-modem": "legacy",
    "jamescherti/gentoo-brother-ql1110nwb": "legacy",
    "jamescherti/cherrybuckle": "legacy",
    "jamescherti/elispcomp": "legacy",
    "jamescherti/caffeine-aw": "legacy",  # Python: legacy
    "jamescherti/outline-yaml.el": "legacy",
    "jamescherti/python-dpc3848v-wifi-modem": "legacy",
    "jamescherti/docker-oddmuse": "legacy",
    # "jamescherti/jamescherti": "misc",
}

IGNORE_GITHUB_REPOSITORIES: set[str] = {
    # My repos: TODO clone them
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
    "jamescherti/gentoo-brother-ql1110nwb",
    "jamescherti/outline-yaml.el",
    "jamescherti/python-dpc3848v-wifi-modem",
    # Forks
    "jamescherti/ace-window",
    "jamescherti/Ananicy",
    "jamescherti/brother-overlay",
    "jamescherti/diminish.el",
    "jamescherti/elmacro",
    "jamescherti/elquery",
    "jamescherti/emacs-csv-mode",
    "jamescherti/emacs-dockerfile-mode",
    "jamescherti/Emacs-erefactor",
    "jamescherti/emacs-focus",
    "jamescherti/emacs-highlight-numbers",
    "jamescherti/emacs-parent-mode",
    "jamescherti/Emacs-Persistent-Overlays",
    "jamescherti/emacs-queue",
    "jamescherti/emacs-shfmt",
    "jamescherti/emacs-toc-org",
    "jamescherti/eterm-256color",
    "jamescherti/evil-paredit",
    "jamescherti/expand-region.el",
    "jamescherti/explain-pause-mode",
    "jamescherti/fasd",
    "jamescherti/fuzzy-finder-el",
    "jamescherti/golden-ratio.el",
    "jamescherti/gruvbox",
    "jamescherti/highlight-indent-guides",
    "jamescherti/Highlight-Indentation-for-Emacs",
    "jamescherti/lsp-mode",
    "jamescherti/molokai",
    "jamescherti/nord-vim",
    "jamescherti/org-bullets",
    "jamescherti/papercolor-theme",
    "jamescherti/password-mode",
    "jamescherti/rainbow-delimiters",
    "jamescherti/rainbow-mode",
    "jamescherti/savefold.el",
    "jamescherti/setup-emacs",
    "jamescherti/sexpdata",
    "jamescherti/shut-up",
    "jamescherti/sphinx-doc.el",
    "jamescherti/throttled",
    "jamescherti/undercover.el",
    "jamescherti/vim",
    "jamescherti/vim-atom-dark",
    "jamescherti/vim-commentary",
    "jamescherti/Vim-Jinja2-Syntax",
    "jamescherti/vim-mru",
    "jamescherti/vim-orgmode",
    "jamescherti/vim-python-pep8-indent",
    "jamescherti/vim-pythonmodeindent",
    "jamescherti/vim-snippets",
    "jamescherti/vim-surround",
    "jamescherti/vim-template",
    "jamescherti/vim-ultisnips",
    "jamescherti/vim-Verdin",
    "jamescherti/vline",
    "jamescherti/WordPress",
    "jamescherti/xterm-color",
}
