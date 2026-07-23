#!/usr/bin/env bash
#
# Author: James Cherti
# URL: https://github.com/jamescherti/jc-dev
#
# Distributed under terms of the MIT license.
#
# Copyright (C) 2004-2026 James Cherti
#
# Permission is hereby granted, free of charge, to any person obtaining a copy
# of this software and associated documentation files (the “Software”), to deal
# in the Software without restriction, including without limitation the rights
# to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
# copies of the Software, and to permit persons to whom the Software is
# furnished to do so, subject to the following conditions:
#
# The above copyright notice and this permission notice shall be included in all
# copies or substantial portions of the Software.
#
# THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
# IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
# FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
# AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
# LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
# OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
# SOFTWARE.
#

# Installed only whe pip --user
# Removed: types-PyYAML vim-client elispcomp
MY_PIP_PACKAGES=(pathaction git-rexec pdfcipher watch-xfce-xfconf
  ultyas git-smartmv batchfetch git-commitflow)

# Installed in the virtualenv
PIP_PACKAGES=()
# PySocks trash-cli
# Removed: types-PyYAML
PIP_PACKAGES+=(mypy ipython cookiecutter
  yamllint ansible ansible-lint molecule coverage)
PIP_PACKAGES+=(twine build wheel keyrings.cryptfile)
PIP_PACKAGES+=(jedi pydantic prompt-toolkit Pygments Jinja2
  requests psutil libtmux colorama html2text)
PIP_PACKAGES+=(pdfminer)
PIP_PACKAGES+=(pandas odfpy openpyxl xlsxwriter lxml Zstandard matplotlib
  pandas-datareader numexpr Bottleneck python-dateutil pytz)
# PIP_PACKAGES+=(sh rope)
PIP_PACKAGES+=(codestyle pyflakes pylint)
# PIP_PACKAGES+=(vim-vint)
PIP_PACKAGES+=(pytest-mock)
PIP_PACKAGES+=(vulture yapf bandit pyre-check black autopep8 flake8
  pydocstyle isort pylint mccabe)

PIP_PACKAGES+=(python-lsp-server pylsp-mypy python-lsp-isort)

# PIP_PACKAGES+=(ocrmypdf pip-autoremove asciinema)
PIP_PACKAGES+=("${MY_PIP_PACKAGES[@]}")

# shellcheck disable=SC2034
REMOVE_PIP_PACKAGES=(pyright pre-commit)
