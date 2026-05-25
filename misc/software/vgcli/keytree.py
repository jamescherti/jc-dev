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
"""A tree of keys."""

from collections import UserDict


class CannotRemoveRoot(Exception):
    """Cannot remove the root key."""

    # pylint: disable=unnecessary-pass
    pass


# pylint: disable=too-many-ancestors
class KeyTree(UserDict):
    """A tree of keys (each key inside of a tree is unique)."""

    def __init__(self):
        """Init the KeyTree."""
        super().__init__()
        self.parent = None
        self.key = None

    def add(self, *args):
        """Add a node to the tree.

        >>> self.add('dir', 'subdir', 'sub-subdir')

        """

        if len(args) < 1:
            raise TypeError('get() takes at least 1 positional argument.')

        tree = self

        for key in args:
            if key not in tree:
                tree[key] = KeyTree()
                tree[key].parent = tree
                tree[key].key = key

            tree = tree[key]

    def remove(self, *args):
        """Remove a node."""

        if len(args) < 1:
            if self.parent:
                # remove itself

                return self.parent.pop(self.key)

            raise CannotRemoveRoot("You cannot remove the root node.")

        key = list(args)[-1]

        return self.getnode(*args).parent.pop(key)

    def remove_keys(self):
        """Empty the tree."""

        for key in self.keys():
            self.pop(key)

        return self

    def getnode(self, *args):
        """Go inside of a sub-tree and return it."""

        if len(args) < 1:
            raise TypeError('get() takes at least 1 positional argument.')

        tree = None

        for tree in self.foreach(*args):
            pass
        assert tree is not None

        return tree

    def foreach(self, *args):
        """For each node yield the KeyTree."""
        tree = self

        for arg in args:
            try:
                tree = tree[arg]
            except KeyError:
                err_mess = "The node {} does not exist" \
                    .format(str(list(args)))
                raise KeyError(err_mess)

            yield tree

    def all(self):
        """Yield all trees inside this tree.

        >>> for tree in self.all():
                print(tree.path)

        """

        for tree in self.values():
            yield tree

            # subtrees

            for subtree in tree.all():
                yield subtree

    @property
    def path(self):
        """List of keys."""
        result = []
        tree = self

        while tree.parent is not None:
            result.insert(0, tree.key)
            tree = tree.parent

        return result

    def __repr__(self):
        """Show a representation of this file."""

        return self.to_str(indent=2)

    def to_str(self, indent=0):
        """Convert the list of directories/files to a string."""
        # Calculate how many parents (used for spaces)
        tree = self.parent
        amount_parents = 0

        while tree is not None:
            tree = tree.parent
            amount_parents += 1

        indent_str = (' ' * amount_parents * indent)

        result = ''

        for key, value in self.items():
            result += str(indent_str) + key + '\n'
            result += str(value)

        return result
