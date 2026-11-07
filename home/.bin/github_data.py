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
    "jamescherti/update-iptables": "shell",
    # Other
    "jamescherti/python-dpc3848v-wifi-modem": "legacy",
    "jamescherti/docker-oddmuse": "legacy",
    "jamescherti/caffeine-aw": "legacy",
    "jamescherti/elispcomp": "legacy",
    "jamescherti/gentoo-brother-ql1110nwb": "legacy",
    # "jamescherti/jamescherti": "misc",
    # "jamescherti/cherrybuckle": "python",
}

ALLOWED_FORKS = {
    "jamescherti/aggressive-indent-mode",
    "jamescherti/aider",
    "jamescherti/ananicy-rules",
    "jamescherti/ansible-collection-hardening",
    "jamescherti/apheleia",
    "jamescherti/avy",
    "jamescherti/awesome-emacs",
    "jamescherti/benchmark-init-el",
    "jamescherti/cape",
    "jamescherti/combobulate",
    "jamescherti/compat",
    "jamescherti/cond-let",
    "jamescherti/consult",
    "jamescherti/corfu",
    "jamescherti/dash.el",
    "jamescherti/diff-hl",
    "jamescherti/dired-hacks",
    "jamescherti/doom-themes",
    "jamescherti/dtrt-indent",
    "jamescherti/dumb-jump",
    "jamescherti/easy-escape",
    "jamescherti/edit-indirect",
    "jamescherti/ef-themes",
    "jamescherti/elisp-refs",
    "jamescherti/emacs",
    "jamescherti/emacs-ansible",
    "jamescherti/emacs-ansible-doc",
    "jamescherti/emacs-basic-mode",
    "jamescherti/emacs-eat",
    "jamescherti/emacs-elisp-autofmt",
    "jamescherti/emacs-elpa",
    "jamescherti/emacs-libvterm",
    "jamescherti/emacs-nongnu-elpa",
    "jamescherti/emacs-undo-fu",
    "jamescherti/emacs-undo-fu-session",
    "jamescherti/Emacs-wgrep",
    "jamescherti/embark",
    "jamescherti/evil",
    "jamescherti/evil-collection",
    "jamescherti/evil-matchit",
    "jamescherti/evil-snipe",
    "jamescherti/evil-surround",
    "jamescherti/f.el",
    "jamescherti/flymake-quickdef",
    "jamescherti/flymake-yamllint",
    "jamescherti/gcmh",
    "jamescherti/ghostel",
    "jamescherti/git-gutter",
    "jamescherti/git-modes",
    "jamescherti/gnome-just-perfection",
    "jamescherti/gnome-shell-extension-caffeine",
    "jamescherti/gnome-tilingshell",
    "jamescherti/gnu_elpa",
    "jamescherti/goto-chg",
    "jamescherti/gptel",
    "jamescherti/groovy-emacs-modes",
    "jamescherti/helpful",
    "jamescherti/hide-minimized",
    "jamescherti/HideActivities",
    "jamescherti/highlight-defined",
    "jamescherti/highlight-sexp",
    "jamescherti/highlight-symbol.el",
    "jamescherti/indent-bars",
    "jamescherti/jenkinsfile-mode",
    "jamescherti/jinja2-mode",
    "jamescherti/lua-mode",
    "jamescherti/marginalia",
    "jamescherti/markdown-mode",
    "jamescherti/markdown-toc",
    "jamescherti/melpa",
    "jamescherti/modus-themes",
    "jamescherti/nerd-icons-corfu",
    "jamescherti/nerd-icons-dired",
    "jamescherti/nerd-icons.el",
    "jamescherti/nongnu_elpa",
    "jamescherti/orderless",
    "jamescherti/org-appear",
    "jamescherti/org-mode",
    "jamescherti/package-lint",
    "jamescherti/paredit",
    "jamescherti/parsedown",
    "jamescherti/php-mode",
    "jamescherti/prescient.el",
    "jamescherti/run-or-raise",
    "jamescherti/s.el",
    "jamescherti/setup-cask",
    "jamescherti/setup-emacs.el",
    "jamescherti/shell-pop-el",
    "jamescherti/straight.el",
    "jamescherti/syntax-highlighting-code-block",
    "jamescherti/transient",
    "jamescherti/treesit-fold",
    "jamescherti/vertico",
    "jamescherti/vimrc-mode",
    "jamescherti/yaml-mode",
    "jamescherti/yasnippet",
    "jamescherti/ztree",
}
