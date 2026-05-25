#!/usr/bin/env python
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
"""Manage a group of 'Virt' virtual machines."""


import logging
import platform
import re
import sys
import timeit
import xml.etree.ElementTree as etree
from subprocess import check_call
from textwrap import indent
from time import sleep

import libvirt

from keytree import KeyTree
from virt import (WAIT_STATE_SECONDS, SnapshotDoesNotExist, SnapshotNotFound,
                  Virt, VirtEngine)

BOOT_SLEEP_BEFORE_PRESS_ENTER = 0.3


class VirtGroupNoCurrentSnapshot(Exception):
    """No current snapshot found."""

    # pylint: disable=unnecessary-pass
    pass


class VirtGroupInconsistentState(Exception):
    """Raised when a VM state is different than the other VMs."""

    def __init__(self, message, inc_domains=None):
        """Init the exception.

        :message: the error message
        :inc_domains: list of inconsistent domain names (needs to be
        a: set, list or None. It will be converted to a set)

        """
        assert isinstance(inc_domains, (list, set, type(None))), \
            "The type of 'inc_domains' needs to be '(list | set | None)'"

        if inc_domains is None:
            self.inc_domains = None
        elif isinstance(inc_domains, list):
            self.inc_domains = set(inc_domains)
        else:
            self.inc_domains = inc_domains

        super().__init__(message)


class VirtGroupTimeout(Exception):
    """A virtual machine was too slow to start or stop."""

    # pylint: disable=unnecessary-pass
    pass


class VirtSelectorNoVirtualMachineFound(Exception):
    """No virtual machine was found by VirtGroup."""

    # pylint: disable=unnecessary-pass
    pass


# pylint: disable=too-many-ancestors
class SnapshotTree(KeyTree):
    """List of snapshots in a tree."""

    def add_snapshot(self, name):
        """Add a snapshot.

        >>> add_snapshot('root-snapshot-name')

        """
        node_path = name.split('-')
        self.add(*node_path)

    def mark_current(self, name, label='  <=== Current'):
        """Add <=== Current to one of them."""
        args = name.split('-')
        tree = self.getnode(*args)

        old_key = tree.key
        new_key = tree.key + label

        if tree.parent:
            tree.parent.pop(old_key)
            tree.parent[new_key] = tree

        self.key = new_key

    def keywords_exists(self, name):
        """Check if the keywords in name exits.

        >>> self.name('lxc-server')
        ['lxc']

        It means the keywords lxc is already.

        """
        all_keywords = self.all_keywords
        result = set()

        node_path = name.split('-')
        for keyword in node_path:
            if keyword in all_keywords:
                result.add(keyword)

        return result

    @property
    def all_keywords(self):
        """Return a set of all keywords."""
        result = set()
        for tree in self.all():
            result = result.union(set(tree.path))

        return result


# pylint: disable=too-few-public-methods
class VirtSelector:
    """Select the Virt inside of a VirtGroup with filters."""

    def __init__(self, virt_engine,
                 pattern=None, start_from=0, limit=None,
                 pattern_blacklist=None,
                 re_flags=re.IGNORECASE):
        """Init the selector.

        Params:
            virt_engine: the VirtEngine instance.

            pattern: the regular expression used to filter the virtual
            machines the class is going to manage. It is going to filter based
            on the virt name. Example: '^ubuntu[0-9]+$' is an
            example.

            start_from: the first machine we will manage The first virtual
            machine's start_from is always 0.  The parameter limit to speficy
            how many machines we will manage starting from 'start_from'.

            limit: how many machines we will manage (None = unlimited),
            starting from start_from.

            pattern_blacklist: if a virtual machine matching 'pattern' is
            found, it is ignored if it is matching 'pattern_blacklist'. It is
            a sort of blacklist.

            re_flags: same as 'flags' parameter in re.match(). It can help you
            specify parameters like re.IGNORECASE.

        """
        assert isinstance(virt_engine, VirtEngine), \
            "The type of 'virt_engine' needs to be 'VirtEngine'"
        assert isinstance(pattern, (type(None), str)), \
            "The type of 'pattern' needs to be 'str'"
        assert isinstance(start_from, int), \
            "The type of 'start_from' needs to be 'int'"
        assert start_from >= 0, "'start_from' needs to be > 0"
        assert isinstance(limit, (type(None), int)), \
            "The type of 'limit' needs to be 'None' or 'int'"
        assert isinstance(pattern_blacklist, (type(None), str)), \
            "The type of 'pattern_blacklist' needs to be 'None' or 'str'"
        assert isinstance(re_flags, type(re.IGNORECASE)), \
            "The type of 're_flags' must be 'RegexFlag'"

        self.virt_engine = virt_engine
        self.pattern = pattern
        self.start_from = start_from
        self.limit = limit
        self.pattern_blacklist = pattern_blacklist
        self.re_flags = re_flags

    @staticmethod
    # TODO: remove domain_xx and replace it with virt_xx
    def sep_domain_from_numbers(domain_name):
        """Separate the domain name from the numbers.

        Example:
        >>> sep_domain_from_numbers('ubuntu101')
        ('ubuntu', '101')

        >>> sep_domain_from_numbers('ubu9ntu101')
        ('ubuntu', '9101')

        :returns: a tuple with (domain_prefix, number)

        """
        alphas = re.sub(r'[0-9]', '', domain_name)
        numbers = re.sub(r'[^0-9]', '', domain_name)
        return (alphas, numbers)

    def new_domain_names(self, virt_group, amount):
        """Get a new virtual machine name (used to create a new VM).

        This function is going to:
        1. Keep only numbers in all taken_names entries
        2. Find the biggest number
        3. Increment the biggest number and return name+number

        Example: if the biggest virtual machine name is ubu1, it is going to
        return ubu2.

        Return: a set() of VM names.

        """
        assert isinstance(virt_group, VirtGroup), \
            "The type of 'virt_group' needs to be 'VirtGroup'"
        assert isinstance(amount, int), \
            "The type of 'amount' needs to be 'int'"

        list_alphas = []
        biggest_number = 0
        number_with_biggest_len = 0
        for virt in virt_group:
            virt_name = virt.name
            if self.is_approved(virt_name):
                alphas, numbers = self.sep_domain_from_numbers(virt_name)
                list_alphas.append(alphas)

                # find the biggest number
                if int(numbers) > biggest_number:
                    biggest_number = int(numbers)
                if len(numbers) > number_with_biggest_len:
                    number_with_biggest_len = len(numbers)

        count_alphas = Counter(list_alphas)
        virt_name = sorted(count_alphas,
                           key=count_alphas.get,
                           reverse=True)[0]

        biggest_number += 1

        result = set()
        for _ in range(amount):
            str_biggest_number = str(biggest_number)
            if number_with_biggest_len > len(str_biggest_number):
                zfill = number_with_biggest_len
                str_biggest_number = str_biggest_number.zfill(zfill)

            result.add(virt_name + str_biggest_number)
            biggest_number += 1

        return result

    def sort_virt_by_name(self, virt_group):
        """Sort the virts using their name + number."""
        assert isinstance(virt_group, VirtGroup), \
            "The type of 'virt_group' needs to be 'VirtGroup'"

        dict_virt = {}
        for virt in virt_group:
            prefix, numbers = self.sep_domain_from_numbers(virt.name)
            dict_virt[prefix + numbers] = virt

        sorted_virts = []
        for fake_virt_name in sorted(dict_virt.keys()):
            sorted_virts.append(dict_virt[fake_virt_name])

        return sorted_virts

    def is_approved(self, domain_name):
        """Check if a virtual machine should be selected or rejected."""
        assert isinstance(domain_name, str), \
            "The type of 'domain_name' needs to be 'str'"

        if self.pattern_blacklist and \
                re.match(self.pattern_blacklist, domain_name, self.re_flags):
            return False

        if self.pattern is None:
            return True
        elif re.match(self.pattern, domain_name, self.re_flags):
            return True

        return False

    def prefixes(self, virt_group):
        """Return a set of prefixes of all virtual machines."""
        assert isinstance(virt_group, VirtGroup), \
            "The type of 'virt_group' needs to be 'libvirt.virConnect'"
        result = set()
        for virt in virt_group:
            domain, _ = self.sep_domain_from_numbers(virt.name)
            result.add(domain)

        return result

    def __repr__(self):
        """Represent the class."""
        return str(self.__dict__)

    def __iter__(self):
        """Iter a libvirt conn using the filters in self."""
        list_domains = {}

        try:
            list_all_domains = self.virt_engine.libvirt.listAllDomains()
        except libvirt.libvirtError as err:
            if err.get_error_code() == libvirt.VIR_ERR_SYSTEM_ERROR or \
                    err.get_error_code() == libvirt.VIR_ERR_INTERNAL_ERROR:
                self.virt_engine.reconnect()
                list_all_domains = self.virt_engine.libvirt.listAllDomains()
            else:
                raise

        for domain in list_all_domains:
            domain_name = domain.name()
            if self.is_approved(domain_name):
                list_domains[domain.name()] = domain
            else:
                continue

        keys = sorted(list_domains.keys())[self.start_from:]
        if self.limit is not None:
            keys = keys[0:self.limit]

        for domain_name in keys:
            yield Virt(domain=list_domains[domain_name],
                       virt_engine=self.virt_engine)

    def __getitem__(self, domain_name):
        """Get a specific domain name."""
        assert isinstance(domain_name, str), \
            "The type of 'domain_name' needs to be 'str'"

        for virt in self:
            if virt.name == domain_name:
                return virt

        err_msg = "No virtual machine was found with " \
            "the domain name: {}".format(domain_name)
        raise VirtSelectorNoVirtualMachineFound(err_msg)


class VirtGroup(object):
    """Virt virtual machines.

    Params:
        :selector: the VirtSelector.

        :libvirt_uri: the libvirt URI. Default value: 'qemu:///system'.

    """

    def __init__(self, selector, libvirt_uri=None):
        """Init the class with a selector (a filter)."""
        assert isinstance(selector, VirtSelector)
        assert isinstance(libvirt_uri, (type(None), str))

        self.selector = selector

        if not list(self):
            err_msg = "No virtual machine was found with " \
                "the selector: {}".format(str(self.selector))
            raise VirtSelectorNoVirtualMachineFound(err_msg)

    def __iter__(self):
        """Yield the different virtual machines."""
        for domain in self.selector:
            yield domain

    def __len__(self):
        """Calculate how many VMs have been found."""
        amount = 0
        for _ in self:
            amount += 1
        return amount

    def stop(self, graceful=True):
        """Stop all virtual machines.

        :graceful: stop the virtual machine gracefully.

        """
        stopped_virt = []
        for virt in self:
            virt.stop(graceful=graceful)
            stopped_virt.append(virt)

        for virt in stopped_virt:
            virt.wait(is_running=False)

    def wait(self, *args, **kwargs):
        """Similar to the function in the class Virt()."""
        for virt in self:
            virt.wait(*args, **kwargs)

    def pop(self, amount=1):
        """Delete the last VM."""
        assert isinstance(amount, int), \
            "The type of 'amount' needs to be 'int'"

        if amount <= 0:
            raise ValueError("The 'amount' needs to be >= 0")

        sorted_virt = self.selector.sort_virt_by_name(self)

        for _ in range(amount):
            virt = sorted_virt.pop()

            virt.stop()
            # TODO: write the Python version
            logging.info("[DELETE-VM] '%s'", virt.name)
            cmd = ['virsh', 'undefine', virt.name,
                   '--snapshots-metadata', '--managed-save',
                   '--remove-all-storage']
            check_call(cmd)

    def _future_pop(self, amount=1):
        """Delete the last VM."""
        # TODO: find a way to replace self.pop with this one (pure Python)
        assert isinstance(amount, int) and amount > 0, \
            "The type of 'amount' needs to be 'int'"
        sorted_virt = self.selector.sort_virt_by_name(self)

        for _ in range(amount):
            virt = sorted_virt.pop()
            domain = virt.domain

            virt.stop()

            # get the image files liked to the domain
            xmldesc = etree.fromstring(domain.XMLDesc())
            for disk in xmldesc.iterfind('./devices/disk/source'):
                path = disk.get('file')
                # TODO: add here something to delete
                logging.info("[WARNING] You need to delete "
                             "this file manually: '%s'", path)

            logging.info("[DELETE-VM] '%s'", domain.name())
            flags = libvirt.VIR_DOMAIN_UNDEFINE_MANAGED_SAVE | \
                libvirt.VIR_DOMAIN_UNDEFINE_SNAPSHOTS_METADATA | \
                libvirt.VIR_DOMAIN_UNDEFINE_NVRAM
            domain.undefineFlags(flags)

    def start(self, wait_for_ssh=False, press_enter=False):
        """Start all virtual machines."""
        # list of virt that have been started
        started_virt = []

        for virt in self:
            if not virt.is_running():
                virt.start()
                started_virt.append(virt)

        # wait until all of them are marked as started
        for virt in started_virt:
            # wait at least WAIT_STATE_SECONDS
            time_start = timeit.default_timer()
            while True:
                if virt.is_running():
                    break

                time_total = int(timeit.default_timer() - time_start)
                if time_total > WAIT_STATE_SECONDS:
                    err_msg = "The VM '{}' was too slow to start." \
                        .format(virt.name)
                    raise VirtGroupTimeout(err_msg)

        if started_virt:
            sleep(BOOT_SLEEP_BEFORE_PRESS_ENTER)

        for virt in self:
            if press_enter:
                virt.send_enter_key()

            if wait_for_ssh:
                virt.wait(port=22)

    def snapshot_remove(self, snapshot_name):
        """Remove a snapshot."""
        for virt in self:
            virt.snapshots[snapshot_name].delete()

    def snapshot_current(self):
        """Return the current snapshot name."""
        result = None
        result_virt = None
        for virt in self:
            try:
                if not virt.snapshots.names:
                    continue

                current_snap_name = virt.snapshots.current.name
            except (SnapshotDoesNotExist,
                    SnapshotNotFound):
                continue

            if result is None:
                result = current_snap_name
                result_virt = virt
            else:
                if result != current_snap_name:
                    err_msg = "'{}' (the current snapshot of '{}') " \
                        "is different than the other VMs (the current " \
                        "snapshot should have been '{}', like the VM '{}')" \
                        .format(current_snap_name, virt.name,
                                result, result_virt.name)
                    raise VirtGroupInconsistentState(err_msg)

        if not result:
            err_msg = "The virtual machines do not have any " \
                "snapshot that is marked as 'current'"
            raise VirtGroupNoCurrentSnapshot(err_msg)

        return result

    def snapshot_save(self, snapshot_name, restore_state=True):
        """Save the virtual machine's state to 'snapshot_name' state."""
        if restore_state:
            state = self.state()

        for virt in self:
            virt.snapshots[snapshot_name].save()

        if restore_state:
            self.state(restore_state=state)

    # TODO: should it be restart or restore_state?
    def snapshot_load(self, snapshot_name, restore_state=True):
        """Load the state 'snapshot_name'."""
        if restore_state:
            state = self.state()

        # the libvirt exceptions
        for virt in self:
            try:
                # TODO: load in all of them before the exception
                # and return inc_domain?
                snapshot = virt.snapshots[snapshot_name]
                snapshot.load()
            except SnapshotDoesNotExist as err:
                raise VirtGroupInconsistentState(str(err))

        if restore_state:
            self.state(restore_state=state)

    def snapshot_names(self):
        """Return a set of all snapshot names."""
        result = set()
        for virt in self:
            for snapshot in virt.snapshots:
                result.add(snapshot.name)

        return result

    def state(self, restore_state=None, wait_port=None):
        """Return a list of states: (virt, virt.is_running() ).

        Params:
            :restore_state: restore the state (poweroff / poweron status)
            using the list retured by self.state() without any argument.

            :wait_port: when restore_state isn't None, wait until all
            started virtual machine's port is open (usually used for SSH).
            It doesn't apply to virtual machines that were stopped.

        """
        if restore_state:
            for virt, is_running in restore_state:
                assert isinstance(virt, Virt), \
                    "The type of 'virt' needs to be 'Virt'"
                assert isinstance(is_running, bool), \
                    "The type of 'is_running' needs to be 'bool'"

                if virt.is_running() != is_running:
                    # is_running is the state the VM should be on
                    if not is_running:
                        virt.stop()

                        port = None
                        is_running = False
                    else:
                        virt.start()

                        port = wait_port
                        is_running = True

                    virt.wait(is_running=is_running,
                              port=port)

        result = []
        for virt in self:
            result.append((virt, virt.is_running()))

        return result

    def reconnect(self):
        """Reconnect the virt engines."""
        for virt in self:
            virt.virt_engine.reconnect()

    def __repr__(self):
        """Show the status of the VMs (running, stopped)."""
        result = ''
        all_snaps = self.snapshot_names()

        if all_snaps:
            # current snapshot
            current_snapshot = None
            try:
                current_snapshot = self.snapshot_current()
            except (VirtGroupInconsistentState,
                    VirtGroupNoCurrentSnapshot):
                pass

            # hierarchy of snapshots
            tree = SnapshotTree()
            for snap_name in all_snaps:
                tree.add_snapshot(snap_name)

            if current_snapshot:
                tree.mark_current(current_snapshot)

            result += 'Snapshots:\n'
            result += '----------\n'
            result += indent(str(tree), ' ' * 4) + '\n'
        else:
            result += 'Snapshots: '
            result += '<NO SNAPSHOTS>\n'

        result += 'State:\n'
        result += '------\n'
        for virt in self:
            cur_dom_snaps = virt.snapshots.names
            missing_snaps = all_snaps - cur_dom_snaps
            str_missing_snaps = ', '.join(missing_snaps)

            if virt.is_running():
                result += '    [RUNNING] ' + virt.name
            else:
                result += '    [STOPPED] ' + virt.name

            if missing_snaps:
                result += ' (missing snapshots: {})'.format(str_missing_snaps)

            result += '\n'

        return result

    def __getitem__(self, domain_name):
        """Select a specific domain name.

        :returns: Virt instance

        """
        for virt in self:
            if virt.name == domain_name:
                return virt

        err_msg = "The virtual machine '{}' wasn't found" \
            .format(domain_name)
        raise VirtSelectorNoVirtualMachineFound(err_msg)
