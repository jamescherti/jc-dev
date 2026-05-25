##!/usr/bin/env python
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
"""Manage a list of accounts. This class is used by Operations()."""


from config import DEFAULT_CURRENCY


class AccountUsed(Exception):
    """Raised if you try to remove an account that is in use."""


class Accounts:
    """Store a list of accounts (used by Operations() class).

    Each account has a currency inside it.

    Example:
    --------
    >>> acc = Accounts(currency="$")
    >>> acc.add("main", "$USD")
    >>> print acc["main"]
    $USD

    >>> acc.add("test", "$USD")
    >>> acc.default = "test"

    """

    def __init__(self, currency=DEFAULT_CURRENCY):
        """Initialize and clear the accounts."""
        self.__default = None
        self.clear(currency=currency)

    def clear(self, currency=DEFAULT_CURRENCY):
        """Delete all accounts."""
        self.currency = currency
        self.accounts = {}
        self.default = None

    def get(self):
        """Get a sorted list of the account names."""
        return sorted(self.accounts)

    def add(self, name, currency=DEFAULT_CURRENCY, default=False):
        """Add a new account.

        Arguments:
        ----------
        default: if default=True, make this new account the default one
        """
        name = self.norm_account_name(name)
        self.accounts[name] = currency
        if self.default is None or default is True:
            # The first one is the default account
            self.default = name

    def modify(self, name, currency=DEFAULT_CURRENCY, default=False):
        """Modify an account."""
        self.add(name, currency, default)

    def exists(self, name):
        """Check if the account exists."""
        name = self.norm_account_name(name)
        return name in self.accounts

    def rename(self, old_name, new_name):
        """Rename the account name from name to a new name.

        Exception:
            KeyError    if new_name exists
        """
        old_name = self.norm_account_name(old_name)
        new_name = self.norm_account_name(new_name)

        if new_name in self.accounts:
            raise KeyError("You can't rename the account '%s' to '%s', "
                           " because the account '%s' exists." %
                           (old_name, new_name, new_name))

        self.accounts[new_name] = self.accounts[old_name]
        if self.default == old_name:
            self.default = None
        del self.accounts[old_name]

    def remove(self, name):
        """Remove an account (KeyError if doesn't exist)."""
        name = self.norm_account_name(name)
        if self.default == name:
            self.default = None

        del self.accounts[name]

    @staticmethod
    def norm_account_name(name):
        """Transform the name to lowercase + no spaces (strip)."""
        assert isinstance(name, str)
        return name.lower().strip()

    @property
    def default(self):
        """Return the default account."""
        return self.__default

    @default.setter
    def default(self, name):
        """Set the default account.

        Exceptions:
        -----------
            IndexError if the account name doesn't exist
        """
        if name is not None:
            name = self.norm_account_name(name)
            if name not in self.accounts:
                raise IndexError("The account '%s' doesn't exist." % name)

        self.__default = name

    def __getitem__(self, name):
        """Get the currency of an existing account."""
        name = self.norm_account_name(name)
        return self.accounts[name]


# vim:ai:et:sw=4:ts=4:sts=4:tw=78:fenc=utf-8
