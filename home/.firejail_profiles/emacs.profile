# Firejail profile for emacs
# Description: GNU Emacs editor
# This file is overwritten after every install/update
# Persistent local customizations
# include emacs.local

# Persistent global definitions
include globals.local

read-only ${HOME}
read-write ${HOME}/src
read-write ${HOME}/.devemacs.d
read-write ${HOME}/.emacs-*
read-write ${HOME}/.emacs.*
read-write ${HOME}/.esa-ssh-agent.pid
read-write ${HOME}/.bash_lastdir
read-write ${HOME}/.*_history
read-write ${HOME}/.fasd_data
read-write ${HOME}/.gnupg_firejail
read-write ${HOME}/.config/git-commitflow

# TODO Fix this issue by moving org-persist somewhere else:
# org-persist-directory
# whitelist ${HOME}/.cache/org-persist
# blacklist ${HOME}/.cache
# blacklist ${HOME}/.cache/thumbnails
# blacklist ${HOME}/.cache/mozilla
# blacklist ${HOME}/.cache/chromium

blacklist ${HOME}/Media
blacklist ${HOME}/Pictures
blacklist ${HOME}/Documents
blacklist ${HOME}/Sync
blacklist ${HOME}/Downloads
blacklist ${HOME}/.var
# blacklist ${HOME}/.thumbnails

# Disabled by disable-common.inc
noblacklist ${PATH}/hostname
blacklist ${HOME}/.ssh

blacklist ${HOME}/.gnupg

#  By default, tmux stores its server sockets in /tmp/tmux-<UID>/default. If you
# enable private-tmp, Emacs will look in its own private /tmp and find nothing.
# You will be unable to attach to existing tmux sessions or share them with the
# rest of your system.
#
# If you want Emacs to interact with your system-wide tmux sessions,
# you must comment out or remove private-tmp.
private-tmp

# This creates a temporary, private directory for ~/.cache.
#
# It prevents Emacs (and any potentially malicious Elisp packages) from reading
# your browser cache, thumbnail cache, or other application data stored in
# ${HOME}/.cache.
#
# If you use mypy, it defaults to storing its cache in .mypy_cache within your
# project folder (which is fine). However, if you have global Python caches in
# ~/.cache, they will be "wiped" inside the sandbox. If your workflow depends on
# those, you would need to whitelist those specific sub-directories.
private-cache

# Standard Text Editing: If you strictly use Emacs for writing Python, Bash, and
# Elisp in a terminal (emacs -nw), you don't need access to physical hardware.
#
# GPU Acceleration: If you use the GUI version of Emacs (especially with pgtk or
# Cairo), it may use the GPU for rendering. private-dev will hide /dev/dri,
# which can lead to graphical lag or fallback to software rendering.
# private-dev

# DBUS: (Optional) Often needed for Emacs to talk to system services
dbus-user none
dbus-system none

# --------------------------------------------
# Same as /etc/firejail/emacs.profile
# --------------------------------------------
# Disable Internet
# net none

# Allows files commonly used by IDEs
include allow-common-devel.inc

# TODO
# This disables paths such as ~/.ssh
# include disable-common.inc

# Reenable ~/.ssh that disable-common.inc disabled
# whitelist ${HOME}/.ssh

# TODO
# It disabled /usr/bin. Too restrictive
# include disable-programs.inc
# Drop all Linux capabilities for the sandboxed application. This increases
# security by limiting what the application can do.
caps.drop all

# Enable netfilter for controlling network access from the sandbox. This adds an
# extra layer of security by filtering network traffic.
netfilter

# Disable access to DVD devices. This is typically not needed for most
# applications and improves security.
nodvd

# Remove group memberships. This prevents the application from using group
# privileges, enhancing security.
nogroups

# Prevent gaining new privileges. This ensures that the application cannot gain
# additional privileges during execution.
nonewprivs

# Drop root privileges if they exist. This ensures the application runs with
# non-root privileges, enhancing security.
noroot

# Disable access to TV devices. This is usually not needed and reduces the
# attack surface.
notv

# Disable access to video devices. This is typically not necessary for most
# applications and enhances security.
novideo

# Allow only specified protocols: Unix, IPv4, and IPv6. This restricts the
# network protocols that the application can use.
protocol unix,inet,inet6

# Enable seccomp (secure computing mode) to restrict the system calls the
# application can make, adding an extra layer of security.
seccomp

# Restrict namespaces to prevent the application from creating or using new
# namespaces, which helps to limit its isolation capabilities.
#
# Why it matters: Many Linux kernel vulnerabilities (privilege escalations) rely
# on manipulating namespaces. By restricting them, you significantly reduce the
# attack surface.
#
# Compatibility: Standard Emacs usage, Python scripts, and mypy do not need to
# create namespaces. Only tools like Docker, Podman, or other "sandboxes within
# the sandbox" would break.
restrict-namespaces
