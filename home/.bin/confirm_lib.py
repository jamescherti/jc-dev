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


from typing import cast

from prompt_toolkit import prompt
from prompt_toolkit.key_binding import KeyBindings, KeyPressEvent


def confirm(message: str = "Continue?", *, default: bool = False) -> bool:
    """Prompt the user for a yes/no decision with a single keypress.

    Args:
        message: The prompt message to display.
        default: The fallback value when pressing Enter.

    Returns:
        True if confirmed, False otherwise.
    """
    bindings = KeyBindings()
    hint = "[Y/n]" if default else "[y/N]"
    full_prompt = f"{message} {hint} "

    @bindings.add("y")
    @bindings.add("Y")
    def _handle_yes(event: KeyPressEvent) -> None:
        event.app.exit(result=True)

    @bindings.add("n")
    @bindings.add("N")
    def _handle_no(event: KeyPressEvent) -> None:
        event.app.exit(result=False)

    @bindings.add("enter")
    def _handle_enter(event: KeyPressEvent) -> None:
        event.app.exit(result=default)

    @bindings.add("c-c")
    def _handle_cancel(event: KeyPressEvent) -> None:
        event.app.exit(exception=KeyboardInterrupt)

    return cast("bool", prompt(full_prompt, key_bindings=bindings))
