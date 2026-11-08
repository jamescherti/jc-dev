#!/usr/bin/env python3
#
# Python Requirements:
#   - PyGithub
#   - PyYAML
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
"""GitHub library."""

import functools
import os
import re
import subprocess
import sys
from collections.abc import Callable, Iterator
from pathlib import Path
from typing import TYPE_CHECKING, ParamSpec, TypeVar, cast

import yaml
from github import Auth, Github
from github.GithubException import GithubException
from github.Repository import Repository

if TYPE_CHECKING:
    from github.AuthenticatedUser import AuthenticatedUser


class GitHubLib:
    """Library class for GitHub configuration and repository information."""

    def __init__(self, token: str | None = None) -> None:
        """
        Initialize GitHubLib and store the GitHub Personal Access Token.

        Args:
            token: Optional GitHub Personal Access Token. If omitted,
                the token is loaded from ~/.secrets/github.yaml.asc or
                the GITHUB_TOKEN environment variable.
        """
        self.token = token if token is not None else self._retrieve_github_token()
        self.auth = Auth.Token(self.token)
        self.github = Github(auth=self.auth)

    @staticmethod
    def _retrieve_github_token() -> str:
        """
        Retrieve the GitHub Personal Access Token.

        Checks ~/.secrets/github.yaml.asc first, decrypts it using GPG, then falls
        back to the GITHUB_TOKEN environment variable.

        Returns:
            The GitHub token string.

        Raises:
            ValueError: If the token cannot be found or decrypted.
        """
        secrets_file = Path.home() / ".secrets" / "github.yaml.asc"

        # Check ~/.secrets/github.yaml.asc first to match documented priority,
        # and raise ValueError on GPG/YAML failures instead of calling sys.exit(1)
        # so callers and @handle_github_errors manage process termination.
        if secrets_file.is_file():
            try:
                result = subprocess.run(
                    ["gpg", "--decrypt", "--quiet", "--batch", str(secrets_file)],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                secrets = yaml.safe_load(result.stdout)
                if isinstance(secrets, dict):
                    raw_token = secrets.get("token")
                    if raw_token and str(raw_token).strip():
                        return str(raw_token).strip()
            except FileNotFoundError as e:
                raise ValueError(
                    f"Cannot decrypt {secrets_file}: 'gpg' command not found"
                ) from e
            except subprocess.CalledProcessError as e:
                err_msg = e.stderr.strip() if e.stderr else str(e)
                raise ValueError(f"Failed to decrypt {secrets_file}: {err_msg}") from e
            except yaml.YAMLError as e:
                raise ValueError(
                    f"Failed to parse decrypted content of {secrets_file}: {e}"
                ) from e

        token = os.environ.get("GITHUB_TOKEN", "").strip()
        if token:
            return token

        raise ValueError(
            "GitHub token not found. Please provide it via ~/.secrets/github.yaml.asc "
            "or the GITHUB_TOKEN environment variable."
        )

    def get_github_token(self) -> str:
        """
        Return the stored GitHub Personal Access Token.

        Returns:
            The GitHub token string.
        """
        return self.token

    def get_github_repo_name(self) -> str:
        """
        Parse the GitHub repository identifier (owner/repo) from the origin remote URL.

        Returns:
            The parsed repository string.

        Raises:
            ValueError: If the remote URL does not match standard GitHub formats.
        """
        result = subprocess.run(
            ["git", "remote", "get-url", "origin"],
            capture_output=True,
            text=True,
            check=True,
        )
        url = result.stdout.strip()

        # Match standard HTTPS, SSH (ssh://), and SCP-like GitHub URLs (including
        # SSH host aliases like github.com-user and optional port numbers).
        match = re.search(
            r"github\.com[^:/]*(?::\d+)?[:/]([^/]+/[^/]+?)(?:\.git)?/?$",
            url,
        )
        if match:
            return match.group(1)

        raise ValueError(f"Could not parse GitHub repository from URL: {url}")

    def create_release(self, tag_name: str, repo_name: str | None = None) -> None:
        """
        Create a GitHub release for the given tag using the PyGithub library.

        Args:
            tag_name: The tag name for the release.
            repo_name: The GitHub repository identifier (e.g., owner/repo). If omitted,
                the origin remote of the current working repository is used.
        """
        if repo_name is None:
            repo_name = self.get_github_repo_name()

        print(f"Creating GitHub release for {repo_name} via API...")
        repo = self.github.get_repo(repo_name)

        repo.create_git_release(
            tag=tag_name,
            name=tag_name,
            message="",
            generate_release_notes=True,
        )
        print("Release created.")

    def list_repos(self, ignore_repos: list[str] | None = None) -> Iterator[Repository]:
        """
        Yield all repositories names owned by the user.

        Args:
            ignore_repos: Optional list of repository full names (owner/repo) to ignore.

        Yields:
            A GitHub repository.
        """
        ignored = set(ignore_repos) if ignore_repos else set()

        user = cast("AuthenticatedUser", self.github.get_user())
        repos = user.get_repos(
            visibility="all",
            affiliation="owner",
            sort="full_name",
            direction="asc",
        )

        for repo in repos:
            repo_name = repo.full_name
            if repo_name not in ignored:
                yield repo


# Type variables used to preserve the exact signature and return type of the decorated
# function:
# - P (ParamSpec): Captures the caller function's parameter specification (positional
#   and keyword arguments) so tools like mypy and IDEs preserve argument autocomplete
#   and validation.
# - R (TypeVar): Captures the return type of the wrapped function so the decorated
#   function reports returning R rather than Any.
P = ParamSpec("P")
R = TypeVar("R")


# Decorator
def handle_github_errors(func: Callable[P, R]) -> Callable[P, R]:
    """Catch configuration, subprocess, and GitHub API errors and exit cleanly."""

    @functools.wraps(func)
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> R:
        try:
            return func(*args, **kwargs)
        except ValueError as err:
            print(f"Value error: {err}", file=sys.stderr)
            sys.exit(1)
        except FileNotFoundError as err:
            print(f"File not found error: {err}", file=sys.stderr)
            sys.exit(1)
        except subprocess.CalledProcessError as err:
            print(f"Command failed: {err}", file=sys.stderr)
            sys.exit(1)
        except GithubException as err:
            print(f"GitHub API error: {err}", file=sys.stderr)
            sys.exit(1)

    return wrapper
