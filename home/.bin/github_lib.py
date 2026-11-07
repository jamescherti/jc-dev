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
"""
GitHub library.
"""

import yaml
import os
import sys
import subprocess
from pathlib import Path





def get_github_token() -> str:
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
