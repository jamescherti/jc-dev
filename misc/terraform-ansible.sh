#!/usr/bin/env bash
#
# Describe: Install the jc-dev development environment
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Distributed under terms of the MIT license.
#
# Copyright (C) 2004-2026 James Cherti
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the "Software"), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

# Exit immediately if a command exits with a non-zero status
set -e
set -u

# Detect the operating system
if [ -f /etc/os-release ]; then
  # shellcheck disable=SC1091
  . /etc/os-release
  OS="$ID"
else
  echo "Unable to detect the operating system."
  exit 1
fi

# Install initial packages
if [ "$OS" = "arch" ]; then
  pacman -Sy --noconfirm \
    sudo \
    python \
    openssh # cronie python-pip
elif [ "$OS" = "debian" ]; then
  apt-get update
  apt-get install -y \
    sudo \
    python3 \
    openssh-server \
    openssh-client # cron python3-pip
elif [ "$OS" = "gentoo" ]; then
  emerge -v \
    app-admin/sudo \
    dev-lang/python \
    net-misc/openssh # sys-process/cronie dev-python/pip
else
  echo "Unsupported OS: $OS"
  exit 1
fi

# Ansible user
groupadd --system ansible
mkdir -p /var/lib/ansible/
useradd --system --home-dir /var/lib/ansible -g ansible ansible
chown -R ansible:ansible /var/lib/ansible/
chmod 700 /var/lib/ansible/

echo "ansible ALL=(ALL) NOPASSWD: ALL" >/etc/sudoers.d/04-ansible
chmod 440 /etc/sudoers.d/04-ansible
