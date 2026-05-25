#!/usr/bin/env python
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
"""Manage a virtual machine."""


import logging
import os
import socket
import timeit
from time import sleep
from xml.dom import minidom as xml_minidom
from xml.sax.saxutils import escape as xml_escape

import libvirt

# In a loop sleep LOOP_SLEEP_SECONDS before trying something again
LOOP_SLEEP_SECONDS = 0.3

# used by virt.wait() to wait until a machine is booted or stopped
WAIT_STATE_SECONDS = 60 * 30


class SnapshotDoesNotExist(Exception):
    """Cannot load a snapshot because it does not exist."""


class SnapshotNotFound(Exception):
    """Raised when a snapshot was not found."""


class VirtSnapshot:
    """Manage one snapshot."""

    def __init__(self, snapshot_name, virt):
        """Init the snapshot."""
        assert isinstance(snapshot_name, str)
        assert isinstance(virt, Virt)
        self.name = snapshot_name
        self.virt = virt

    def save(self):
        """Save the snapshot."""
        # find the previous snapshot
        create_tmp = False
        try:
            previous_snapshot = self.virt.snapshots.current.name
            if previous_snapshot == self.name:
                logging.info("[INFO-SNAPSHOT-SAVE] The current snapshot "
                             "'%s' will be replaced using a tmp snapshot.",
                             self.name)
                create_tmp = True
        except SnapshotNotFound:
            previous_snapshot = None

        if not create_tmp:
            # a simple save as a new name
            self.save_as(name=self.name)
        else:
            tmpname = 'TMP-' + self.name
            try:
                # Instead of save_as/delete/save_as, use save_as/delete/RENAME
                self.save_as(name=tmpname)
                self.delete()
                self.save_as(name=self.name)
            finally:
                self.virt.snapshots[tmpname].delete()

    def save_as(self, name):
        """Save the snapshot as a new name."""
        # delete the snapshot
        self.virt.snapshots[name].delete()

        # Save the snapshot
        logging.info("[SAVE-SNAPSHOT] '%s' ---> '%s'",
                     self.virt.name, name)
        xml = "<domainsnapshot><name>{}</name></domainsnapshot>" \
            .format(xml_escape(name))
        self.virt.domain.snapshotCreateXML(xml)

        # load it again so it is going to be the current one (it is going
        # to be done in the tmp one too)
        self.virt.snapshots[name].load()

    def load(self):
        """Load a snapshot."""
        if self.exists():
            # load the snapshot
            logging.info("[SNAPSHOT-LOAD] '%s' (VM) <--- '%s' "
                         "(Snapshot)", self.virt.name,
                         self.name)
            # TODO: I can use
            # https://libvirt.org/html/libvirt-libvirt-domain-snapshot.html
            # VIR_DOMAIN_SNAPSHOT_REVERT_RUNNING in @flags overrides the
            # snapshot state to guarantee a running domain after the revert;
            # or including VIR_DOMAIN_SNAPSHOT_REVERT_PAUSED
            flags = libvirt.VIR_DOMAIN_SNAPSHOT_REVERT_FORCE
            self.virt.domain.revertToSnapshot(self._libvirt_snapshot,
                                              flags=flags)
        else:
            err_msg = "'{}' (VM) <--- '{}' " \
                "(the snapshot does not exist)" \
                .format(self.virt.name, self.name)
            raise SnapshotDoesNotExist(err_msg)

    def delete(self):
        """Delete the snapshot."""
        if not self.exists():
            return

        logging.info("[SNAPSHOT-DELETE] '%s' removed from '%s'",
                     self.name, self.virt.name)
        self._libvirt_snapshot.delete()

    @property
    def _libvirt_snapshot(self):
        """Return the snapshot instance."""
        return self.virt.domain.snapshotLookupByName(self.name)

    def exists(self):
        """Return True if the snapshot exist."""
        return self.name in self.virt.domain.snapshotListNames()

    def __repr__(self):
        """Return the snapshot name."""
        return self.name


# pylint: disable=too-few-public-methods
class VirtListSnapshots:
    """Manage a list of Snapshots."""

    def __init__(self, virt):
        """Helper to have access to a list of snapshots."""
        self.virt = virt

    def __getitem__(self, snapshot_name):
        """Get a snapshot using its name."""
        return VirtSnapshot(snapshot_name=snapshot_name, virt=self.virt)

    def __iter__(self):
        """Iter over all snapshots."""
        for snapshot_name in self.virt.domain.snapshotListNames():
            yield VirtSnapshot(snapshot_name=snapshot_name, virt=self.virt)

    def __repr__(self):
        """Return a list of snapshots."""
        return ', '.join([snap.name for snap in self])

    @property
    def names(self):
        """Return a set of snapshot names."""
        result = set()
        for snapshot in self:
            result.add(snapshot.name)
        return result

    @property
    def current(self):
        """Return the current snapshot (or None if not found)."""
        try:
            current_snap = self.virt.domain.snapshotCurrent()
        except libvirt.libvirtError:
            # The snapshot was not found
            err_msg = "The domain '{}' does not have a current snapshot" \
                .format(self.virt.name)
            raise SnapshotNotFound(err_msg)

        snapshot_name = current_snap.getName()
        return VirtSnapshot(snapshot_name=snapshot_name, virt=self.virt)


class VirtEngineLinkConnectVirtualizerFailed(Exception):
    """Failed to connect to libvirt."""

    # pylint: disable=unnecessary-pass
    pass


class VirtEngine:
    """Libvirt Connection."""

    def __init__(self, libvirt_uri=None):
        self.libvirt = None  # used by self.__del__()
        self.libvirt_uri = libvirt_uri

        if not self.libvirt_uri:
            try:
                self.libvirt_uri = os.environ['LIBVIRT_DEFAULT_URI'].strip()
            except KeyError:
                # default value
                self.libvirt_uri = 'qemu:///system'

        self.reconnect()

    def reconnect(self):
        """Reconnect to the engine."""
        self.__del__()
        logging.debug('[RECONNECT] reconnect to libvirt')
        self.libvirt = libvirt.open(self.libvirt_uri)

    def __del__(self):
        """Close libvirt connection."""
        if self.libvirt:
            self.libvirt.close()
            self.libvirt = None

        logging.debug('[CLEANUP] libvirt was closed successfully')

    def __repr__(self):
        """String representation."""
        return self.libvirt.getURI()


class VirtTimeout(Exception):
    """A timeout occurred."""


class VirtNotFound(Exception):
    """The virtual machine was not found."""


class Virt:
    """A libvirt virtual machine."""

    def __init__(self, domain, virt_engine):
        """Init the virtual machine."""
        assert isinstance(domain, (str, libvirt.virDomain)), \
            "The type of 'domain' needs to be 'libvirt.virDomain'"
        assert isinstance(virt_engine, VirtEngine), \
            "The type of 'virt_engine' needs to be 'VirtEngine'"

        self.domain = None
        self.virt_engine = virt_engine

        if isinstance(domain, str):
            # simple selector (it is better to use VirtSelector)
            for item in self.virt_engine.libvirt.listAllDomains():

                if item.name() == domain:
                    self.domain = item

            if not self.domain:
                err_msg = "The virtual machine '{}' wasn't found." \
                    .format(domain)
                raise VirtNotFound(err_msg)
        else:
            self.domain = domain

    def is_running(self):
        """Return True when the virtual machine is running."""
        state, _ = self.domain.state()
        return state == libvirt.VIR_DOMAIN_RUNNING

    @property
    def name(self):
        """Return the domain name."""
        return self.domain.name()

    def stop(self, graceful=True):
        """Stop the virtual machine."""
        if self.is_running():
            logging.info('[STOP] %s', self.name)
            if graceful:
                self.domain.destroyFlags(libvirt.VIR_DOMAIN_DESTROY_GRACEFUL)
            else:
                self.domain.destroy()

            self.wait(is_running=False)

    def start(self):
        """Create an instance of the domain."""
        if not self.is_running():
            logging.info('[START] %s', self.name)
            self.domain.create()
            self.wait(is_running=True)

    # pylint: disable=too-many-branches
    def wait(self, port=None, is_running=None, timeout=WAIT_STATE_SECONDS):
        """Wait until a port is open or it is running/not running."""
        assert isinstance(port, (type(None), int)), \
            "The type of 'port' needs to be 'int' or 'bool'"
        assert isinstance(is_running, (type(None), bool)), \
            "The type of 'is_running' needs to be 'bool' or None"

        if port is None and is_running is None:
            raise ValueError('port or is_running needs to be specified')

        if port:
            is_running = True
            self.start()

        # wait until it is running / not running
        if is_running is not None and is_running != self.is_running():
            logging.info("[WAIT] waiting until '%s' is %s.",
                         self.name,
                         'started' if is_running else 'stopped')

            time_start = timeit.default_timer()
            while True:
                if is_running == self.is_running():
                    break

                time_total = int(timeit.default_timer() - time_start)
                if time_total > timeout:
                    err_msg = "The VM '{}' was too slow to {}." \
                        .format(self.name,
                                'start' if is_running else 'stop')
                    raise VirtTimeout(err_msg)

        # wait until a port is open
        if port is not None:
            # waiting for the IP
            time_start = timeit.default_timer()
            logging.info("Trying to detect the IP of '%s'",
                         self.name)
            while True:
                ips = list(self.get_ip_addr())
                if ips:
                    # success!
                    break

                time_total = int(timeit.default_timer() - time_start)
                if time_total > timeout:
                    err_msg = "Unable to detect the IPs of " \
                        "the virtual machine '{}'".format(self.name)
                    raise VirtTimeout(err_msg)

                sleep(LOOP_SLEEP_SECONDS)

            # waiting for the port 22
            ip_address = ips[0]
            logging.info("[WAIT] waiting for the port '%s' (%s / %s)",
                         port, self.name, ip_address)

            time_start = timeit.default_timer()
            while True:
                sock = socket.socket(socket.AF_INET, socket.SOCK_STREAM)
                result = sock.connect_ex((ip_address, port))
                if result == 0:
                    break

                time_total = int(timeit.default_timer() - time_start)
                if time_total > timeout:
                    err_msg = "The port '{}' is still closed on '{}' " \
                        "(even after {} seconds)" \
                        .format(port, self.name, timeout)
                    raise VirtTimeout(err_msg)

                sleep(LOOP_SLEEP_SECONDS)

    def send_enter_key(self):
        """Send the enter key."""
        self.domain.sendKey(libvirt.VIR_KEYCODE_SET_LINUX, 0, [28], 1)

    def __repr__(self):
        """String representation of a virtual machine."""
        return self.name

    @property
    def snapshots(self):
        """Iterate over all snapshots."""
        return VirtListSnapshots(virt=self)

    def restore_running_state(self, is_running):
        """Make sure 'self.is_running()' matches the argument 'is_running'."""
        assert isinstance(is_running, bool), \
            "The type of 'is_running' needs to be 'bool'"
        if is_running != self.is_running():
            if is_running:
                self.start()
            else:
                self.stop()

    def get_network_interfaces(self):
        """Return network interfaces.

        :return: {'MAC_ADDRESS': ['ip': 'x.x.x.x', 'prefix': 16]}.

        """
        result = {}

        # get the network interfaces
        opt_lease = libvirt.VIR_DOMAIN_INTERFACE_ADDRESSES_SRC_LEASE
        network_interfaces = self.domain.interfaceAddresses(opt_lease).items()
        for _, value in network_interfaces:
            hwaddr = value['hwaddr']
            try:
                item = result[hwaddr]
            except KeyError:
                result[hwaddr] = []

            for item in value['addrs']:
                result[hwaddr].append({'ip': item['addr'],
                                       'prefix': item['prefix']})

        return result

    def get_network_names(self):
        """Get the network names to which the virtual machine is connected."""
        result = {}
        domain_xml = self.domain.XMLDesc()
        pdomain_xml = xml_minidom.parseString(domain_xml)
        for item_domain in pdomain_xml.getElementsByTagName('domain'):
            for item_dev in item_domain.getElementsByTagName('devices'):
                for item_iface in item_dev.getElementsByTagName('interface'):
                    if item_iface.getAttribute('type') != 'network':
                        continue

                    set_mac = set()
                    set_bridge = set()

                    # get the mac
                    for item in item_iface.getElementsByTagName('mac'):
                        if item.hasAttribute('address'):
                            set_mac.add(item.getAttribute('address'))

                    for item in item_iface.getElementsByTagName('source'):
                        if item.hasAttribute('network'):
                            set_bridge.add(item.getAttribute('network'))

                    for item_bridge in set_bridge:
                        try:
                            result[item_bridge]
                        except KeyError:
                            result[item_bridge] = set()

                        for item_mac in set_mac:
                            result[item_bridge].add(item_mac)

        return result

    def get_ip_addr(self, ip_startswith=None):
        """Return a set of IPs assigned by the DHCP.

        :ip_startswith: return IPs that start with this string.
        :pingable: return only pingable addresses.

        """
        result = set()

        for _, value in self.get_network_interfaces().items():
            for ip_address in value:
                ip_address = ip_address['ip']

                if ip_startswith:
                    if ip_address.startswith(ip_startswith):
                        result.add(ip_address)
                else:
                    result.add(ip_address)

        return result
