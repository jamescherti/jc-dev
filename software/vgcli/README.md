# Virt Group CLI

A command-line interface to manage a group of libvirt virtual machines.

# Commands

- start: Start all virtual machines.
- stop: Stop all virtual machines.
- revert: Load the snapshot for all virtual machines.
- save: Save a snapshot across all virtual machines.
- bootstrap: Install the files and packages required by the current snapshot.

# Usage

## Using SSH
```shell
export LIBVIRT_DEFAULT_URI=qemu+ssh://root@SERVER/system
