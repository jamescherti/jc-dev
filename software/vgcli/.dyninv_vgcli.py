#!/usr/bin/env python
# -*- coding: utf-8 -*-
#
# Ansible Dynamic Inventory.
#
# Doc about inventories:
# http://docs.ansible.com/ansible/dev_guide/developing_inventory.html
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
"""Ansible Dynamic inventory for VirtGroups."""


import argparse
import json
import sys

from vgcli import INTERNAL_NETWORK
from virtgroup import VirtGroup, VirtSelector


class LibvirtInventory(object):
    """Inventory for Ansible and Libvirt."""

    def __init__(self):
        """Init the dynamic inventory."""
        self.inventory = {'_meta': {'hostvars': {}}}
        self.parse_cli_args()

        if self.args.list:
            self.get_inv()

        print(json.dumps(self.inventory))

    def get_inv(self):
        """Read the virtual machines and separate them in categories."""
        selector = VirtSelector(pattern=r'.*')
        virt_group = VirtGroup(selector=selector,
                               ip_startswith=INTERNAL_NETWORK)

        for virt in virt_group:
            virt_name = virt.name
            name, numbers = selector.sep_domain_from_numbers(virt_name)

            if name not in self.inventory:
                self.inventory[name] = []

            if name in self.inventory[name]:
                continue

            self.inventory[name].append(virt_name)

    def parse_cli_args(self):
        """Parse the args."""
        parser = argparse.ArgumentParser()
        parser.add_argument('--list', action='store_true')
        parser.add_argument('--host', action='store')
        self.args = parser.parse_args()


def main():
    """The program starts here."""
    LibvirtInventory()
    sys.exit(0)


if __name__ == '__main__':
    main()
