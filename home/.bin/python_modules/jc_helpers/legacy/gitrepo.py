#!/usr/bin/env python
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
#
#
# TODO: GitWrapper check if git is in path the if the version
#       is all right?
#
"""GIT wrapper and GIT repository manager."""


import logging
import os

from .cmdwrapper import CmdWrapper, CmdWrapperError


class GitRepoException(Exception):
    """Exception raised when a GitRepo function fails."""

    def __init__(self, error_msg):
        """Store the git errors (stdout, stderr, returncode).

        error_msg: the Python exception's error message.

        """
        self.error_msg = error_msg
        super().__init__(self.error_msg)

    def __repr__(self):
        return self.error_msg


class GitWrapper(CmdWrapper):
    """Wrapper around the Git command-line utility."""

    def __init__(self, git_command: str = "git"):
        """Init the Git wrapper."""
        super().__init__(git_command)

        # Remove all GIT_ variables from the environment (to avoid unexpected
        # behaviour). For example: if you run git inside of a git hook, some
        # environment variables could be set (related to Git hooks)
        for key, value in os.environ.items():
            if key.startswith('GIT_'):
                logging.debug('[GIT] IGNORED ENVIRONMENT VARIABLE: %s=%s',
                              key, value)
                self.env.pop(key)

        # Force all output to be in plain ASCII english
        self.env['LANG'] = 'C'
        self.env['LANGUAGE'] = 'C'

        # GIT_TERMINAL_PROMPT=0 will as git to never prompt on the terminal
        # (e.g., when asking for HTTP authentication).
        self.env['GIT_TERMINAL_PROMPT'] = '0'

        # Disable the editor
        self.env['GIT_EDITOR'] = 'false'


class GitRepo:
    """Manage a git repository."""

    def __init__(self, local_path, git_wrapper=None):
        """Init a git repository.

        :local_path: the directory where the repository is cloned
        :git_uri: the URL to the remote repository
        :git_wrapper: a GitWrapper() instance (or None if you want the
        class to instanciate a new one).

        """
        self._logger = logging.getLogger(self.__class__.__name__)

        if git_wrapper:
            assert isinstance(git_wrapper, GitWrapper)
            self._git = git_wrapper
        else:
            self._git = GitWrapper()

        self._local_path = None  # first Init

    @property
    def local_path(self):
        """Get the local path."""
        return self._local_path

    @local_path.setter
    def local_path(self, path):
        """Modify the local path."""
        self._local_path = path
        self._git = self._git.copy(cwd=path)

    def clone(self, remote_url, clone_args=None):
        """Clone the repository in the local path 'self.local_path'.

        :remote_url: the URL of the remote GIT repository
        :clone_args: a list of git clone options (see: man git-clone).
        For example: clone_args=['--recursive']

        """
        if self._valid_repo:
            # get the remote URL of origin
            real_remote_url = self.remote_url()
            if remote_url != real_remote_url:
                raise GitRepoException("the remote URL of the local GIT "
                                       "repository '{}' needs to be '{}'"
                                       " (instead of '{}')."
                                       .format(self.local_path, remote_url,
                                               real_remote_url))

            self._logger.debug("[GIT-CHECK] Success: the remote URL of "
                               "'%s' is '%s'", self.local_path, real_remote_url)

            logging.debug('[IGNORED] git clone %s %s',
                          self.local_path, remote_url)

            return CmdResult(stdout='Already up-to-date.',
                             stderr='',
                             returncode=0)
        else:
            if not clone_args:
                clone_args = []
            else:
                assert isinstance(clone_args, list)

            abs_local_path = os.path.abspath(self.local_path)
            args = ['clone'] + clone_args + [remote_url, abs_local_path]

            git = self._git.copy(cwd='/')
            return git(*args).result    # return stdout, stderr and returncode

    def get_config(self, var):
        """Get a Git config variable's value.

        Equivalent to: git config --get <var>
        """
        try:
            value = self('config', '--get', var).stdout.firstline
        except CmdWrapperError:
            return None
        else:
            return value

    def get_branches(self):
        """Get all branches in a list."""
        git_result = self("for-each-ref", "--format=%(refname:short)")

        result = []
        for line in git_result.stdout.lines:
            result.append(line)

        return result

    def __call__(self, *args, **kwargs):
        """Run git inside inside the git repository.

        params
            kwargs['_bg']: True to run the command in background instead
                           of waiting until it ends.

        """
        if '_bg' in kwargs and kwargs['_bg']:
            return self._git(*args, **kwargs)
        else:
            return self._git(*args, **kwargs).wait()

    def remote_url(self, name='origin'):
        """Return the remote origin URL."""
        return self('ls-remote', '--get-url', name).stdout.firstline

    def set_branch(self, branch, verify=True):
        """Change the branch."""
        self('checkout', '-q', branch)

        branch_all = self.branch_all
        if verify and (branch not in branch_all):
            raise GitRepoException("Cannot switch to the "
                                   "branch '{}' in '{}'"
                                   " (after the branch switch, these are "
                                   "the detected branch names: {})"
                                   .format(branch, self.local_path,
                                           branch_all))

    @property
    def branch(self):
        """Return the branch name (short version)."""
        stdout = self('symbolic-ref', '--short', 'HEAD').stdout
        return stdout.firstline.strip()

    @property
    def branch_all(self):
        """List of all branch names (name, tags/name, refs/head/xxx...)."""
        result = set()

        # Regular branches:
        # =================
        # symbolic-ref HEAD: returns refs/heads/master
        #
        # symbolic-ref --short HEAD: returns master
        #
        # tags:
        # ====
        # name-rev --name-only HEAD: get the branch name (long name like #
        #                            tags/v2.2.11)
        # describe --long.*: if the branch is tagged, get the long tag name
        cmds = [['symbolic-ref', 'HEAD'],
                ['symbolic-ref', '--short', 'HEAD'],
                ['name-rev', '--name-only', 'HEAD'],
                ['describe', '--tags', '--exact-match', 'HEAD'],
                ['describe', '--long', '--tags', '--exact-match', 'HEAD']]

        for cmd in cmds:
            try:
                # TODO: should I parse all lines? It looks like it isn't
                # needed.
                for line in self(*cmd).stdout.lines:
                    line = line.strip()
                    if line == '':
                        continue

                    result.add(line)
            except CmdProcError:
                pass

        return result

    @property
    def is_branch_tag(self):
        """Return True if the current branch is a tag."""
        try:
            self.branch
        except CmdWrapperError:
            # probably a tags/branch
            return False
        else:
            return True

    def set_commit_ref(self, commit_ref):
        """Change the commit ref."""
        if commit_ref == self.commit_ref:
            # already done
            logging.debug("[IGNORED] commit_ref already '%s'",
                          commit_ref)
            return

        self('reset', '-q', '--hard', commit_ref)

        # check the commit_ref
        if commit_ref != self.commit_ref:
            raise GitRepoException("Cannot switch to the "
                                   "commit ref '{}' in '{}'"
                                   .format(commit_ref, self.local_path))

    @property
    def commit_ref(self):
        """Get the commit ref of HEAD."""
        return self('rev-parse', '--verify', 'HEAD') \
            .stdout.firstline

    @property
    def _valid_repo(self):
        """Return True if the current repository is a Git repository."""
        # TODO: use a git command to validate the repo the first time only
        if os.path.exists(os.path.join(self.local_path, '.git')):
            return True

        return False

    def is_dirty(self):
        """Return True when the repo has untracked, modified, deleted files."""
        raise NotImplementedError

    def modified_files(self, diff_filter='ACDMRTUXB'):
        """Return the files that were modified and are part of the repository.

        :diff_filter: the same parameter as --diff-filter for the command

        Snippet from man git-diff(1), --diff-filter option:
        "Select only files that are Added (A), Copied (C), Deleted (D),
        Modified (M), Renamed (R), have their type (i.e. regular file,
        symlink, submodule, ...) changed (T), are Unmerged (U), are
        Unknown (X), or have had their pairing Broken (B). Any
        combination of the filter characters (including none) can be
        used. When * (All-or-none) is added to the combination, all paths
        are selected if there is any file that matches other criteria in
        the comparison; if there is no file that matches other criteria,
        nothing is selected.""

        """
        return self('diff', '--name-only', '--diff-filter',
                    diff_filter, 'HEAD').stdout.lines

    def untracked_files(self, exclude_gitignore=True):
        """Return a list of untracked files.

        :exclude_gitignore: exclude the untracked files that are in .gitignore

        """
        args = ['ls-files', '--others']
        if exclude_gitignore:
            args.append('--exclude-standard')
        return self(*args).stdout.lines
