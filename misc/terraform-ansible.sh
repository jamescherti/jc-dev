#!/usr/bin/env bash

# Exit immediately if a command exits with a non-zero status
set -e

# Detect the operating system
if [ -f /etc/os-release ]; then
  . /etc/os-release
  OS="$ID"
else
  echo "Unable to detect the operating system."
  exit 1
fi

# Install initial packages
if [ "$OS" = "arch" ]; then
  pacman -Sy --noconfirm sudo python
elif [ "$OS" = "debian" ]; then
  apt-get update
  apt-get install -y sudo python3
elif [ "$OS" = "gentoo" ]; then
  emerge -v app-admin/sudo dev-lang/python
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

# Work
groupadd --gid 1000 work
# Added -m to ensure the home directory is actually created
useradd --uid 1000 --home-dir /home/work -m -g work work

# Replace interactive vim with echo to automate file creation
echo "work ALL=(ALL) ALL" >/etc/sudoers.d/02-user_work
chmod 440 /etc/sudoers.d/02-user_work

echo "ansible ALL=(ALL) NOPASSWD: ALL" >/etc/sudoers.d/04-ansible
chmod 440 /etc/sudoers.d/04-ansible

# Expect is for esa
if [ "$OS" = "arch" ]; then
  pacman -S --noconfirm python-pip openssh expect ansible cronie
elif [ "$OS" = "debian" ]; then
  apt-get install -y python3-pip openssh-server openssh-client expect ansible cron
elif [ "$OS" = "gentoo" ]; then
  emerge -v dev-python/pip net-misc/openssh dev-tcltk/expect app-admin/ansible sys-process/cronie
fi
