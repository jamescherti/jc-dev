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

from typing import Any, Callable

import jinja2


class Variables(dict):
    """Store and manage variables.

    This class that inherits from the built-in dict class. It is designed to
    provide a convenient way to store and manage variables as key-value pairs,
    where the keys serve as variable names, and the corresponding values
    represent the variable values. This class is particularly useful when you
    need to dynamically create, access, or modify variables within a
    dictionary-like structure.

    """


class TemplateRendererError(Exception):
    """Exception raised by a renderer (e.g. Jinja2 Renderer)."""


class TemplateRenderer:
    def __init__(self, variables: Variables):
        self.variables = variables

    def _render_string(self, string: str) -> str:
        """Render a string."""
        return string

    def render(self, value: Any) -> Any:
        """Recursively render any value using the templating engine.

        This method recursively renders all the values within the input
        'value'. If 'value' is a string, it is rendered, and the rendered
        string is returned.

        If 'value' is a list or dictionary, it iterates through all its values,
        and if any value is a string, it is rendered using using the templating
        engine. A new dictionary is then returned containing the rendered list
        or dictionary.

        Args:
            value (Any): The value to be rendered.

        Returns:
            Any: The rendered 'value'. If 'value' is a string, a rendered
            string is returned. If 'value' is a list or a dictionary, a new
            dictionary is returned containing the rendered values.

        """
        if isinstance(value, str):
            return self._render_string(value)
        elif isinstance(value, dict):
            return {key: self.render(cur_value)
                    for key, cur_value in value.items()}
        elif isinstance(value, list):
            return [self.render(item) for item in value]
        else:
            return value


class Jinja2Renderer(TemplateRenderer):
    def __init__(self, variables: Variables):
        super().__init__(variables=variables)
        self._env = jinja2.Environment(
            loader=jinja2.BaseLoader,  # type: ignore
            undefined=jinja2.StrictUndefined,
            autoescape=False,
        )

    def set_filter(self, name: str, method: Callable):
        """Set a custom Jinja2 filter method."""
        self._env.filters[name] = method

    def _render_string(self, string: str) -> str:
        """Render a Jinja2 string recursively."""
        previous = string
        while True:
            j2_template = self._env.from_string(string)

            try:
                string = j2_template.render(self.variables)
            except (jinja2.exceptions.TemplateSyntaxError,
                    jinja2.exceptions.UndefinedError) as err:
                raise TemplateRendererError(str(err)) from err

            if string == previous:
                return string
            else:
                previous = string
