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

import os
import subprocess
import sys
from pathlib import Path

import yaml
from github import Auth, Github
from github.GithubException import GithubException


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

    @staticmethod
    def _retrieve_github_token() -> str:
        """
        Retrieve the GitHub Personal Access Token.

        Checks ~/.secrets/github.yaml.asc first, decrypts it using GPG, then falls
        back to the GITHUB_TOKEN environment variable.

        Returns:
            The GitHub token string.

        Raises:
            ValueError: If the token cannot be found in either location.
        """
        token = os.environ.get("GITHUB_TOKEN")
        if token:
            return token

        secrets_file = Path.home() / ".secrets" / "github.yaml.asc"

        if secrets_file.is_file():
            try:
                result = subprocess.run(
                    ["gpg", "--decrypt", "--quiet", "--batch", str(secrets_file)],
                    capture_output=True,
                    text=True,
                    check=True,
                )
                secrets = yaml.safe_load(result.stdout)
                if secrets and isinstance(secrets, dict) and "token" in secrets:
                    return str(secrets["token"])
            except subprocess.CalledProcessError as e:
                print(f"Failed to decrypt {secrets_file}: {e.stderr}")
                sys.exit(1)
            except yaml.YAMLError as e:
                print(f"Failed to parse decrypted content of {secrets_file}: {e}")
                sys.exit(1)

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
        url = url.removesuffix(".git")

        if "github.com:" in url:
            return url.split("github.com:")[1]
        if "github.com/" in url:
            return url.split("github.com/")[1]

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
        auth = Auth.Token(self.token)
        gh = Github(auth=auth)
        repo = gh.get_repo(repo_name)

        repo.create_git_release(
            tag=tag_name,
            name=tag_name,
            message="",
            generate_release_notes=True,
        )
        print("Release created.")

    def list_public_source_repos(self, ignore_repos: list[str] | None = None) -> None:
        """
        Fetch and print all public, non-fork repositories owned by the authenticated user.

        Args:
            ignore_repos: Optional list of repository full names (owner/repo) to ignore.
        """
        auth = Auth.Token(self.token)
        gh = Github(auth=auth)

        ignored = set(ignore_repos) if ignore_repos else set()

        try:
            user = gh.get_user()
            repos = user.get_repos(
                visibility="public",
                affiliation="owner",
                sort="full_name",
                direction="asc",
            )

            count = 0
            for repo in repos:
                repo_name = repo.full_name
                if not repo.private and not repo.fork and repo_name not in ignored:
                    print(f"{repo_name}")
                    count += 1
        except GithubException as err:
            print(f"GitHub API error: {err}")
            sys.exit(1)
