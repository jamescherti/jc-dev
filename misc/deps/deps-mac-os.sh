#!/usr/bin/env bash
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
# Disable animations:
# https://github.com/kevinSuttle/macOS-Defaults/blob/master/.macos
#
# Software:
# ---------
# Virtual box: https://www.virtualbox.org/wiki/Downloads
# Vagrant: https://developer.hashicorp.com/vagrant/downloads?product_intent=vagrant
# Mac Ports: https://www.macports.org/
# iTerm 2: https://iterm2.com/
# Alt-tab: https://alt-tab-macos.netlify.app/
# Caffeine: https://intelliscapesolutions.com/apps/caffeine
# Karabiner: https://karabiner-elements.pqrs.org/
#

set -euf -o pipefail

# You can install compilers and supporting utilities with Xcode. First,
# download Xcode from the Mac App Store, and then run this in the terminal to
# install Xcode's command-line tools:
# xcode-select --install

sudo port install python311 \
  p7zip bash fzf git rsync ripgrep openssh \
  coreutils gnutar gawk gnutls gsed findutils gindent grep \
  shfmt tmux fd sd parallel cmake watch htop \
  sdcv cloc bash-completion aspell aspell-dict-en fdupes tig \
  ncdu jq libvterm git-delta ipcalc enchant

sudo port install shellcheck borgbackup
sudo port install gnupg2

sudo port install emacs +nativecomp +x11 +treesitter -dbus -xwidgets -imagemagick
sudo port install emacs-mac-app-devel +nativecomp +treesitter

sudo port install tree-sitter-python tree-sitter-yaml tree-sitter-bash \
  tree-sitter-javascript tree-sitter-elisp tree-sitter-markdown \
  tree-sitter-dockerfile

sudo port install vim +python311 +huge

sudo port select --set python python311
sudo port select --set python3 python311
