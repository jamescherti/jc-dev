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

if type -P python3.13 &>/dev/null; then
  # shellcheck disable=SC2034
  PYTHON_VERSION=3.14
elif type -P python3.13 &>/dev/null; then
  # shellcheck disable=SC2034
  PYTHON_VERSION=3.13
elif type -P python3.12 &>/dev/null; then
  # shellcheck disable=SC2034
  PYTHON_VERSION=3.12
elif type -P python3.11 &>/dev/null; then
  # shellcheck disable=SC2034
  PYTHON_VERSION=3.11
elif type -P python3.10 &>/dev/null; then
  # shellcheck disable=SC2034
  PYTHON_VERSION=3.10
elif type -P python3.9 &>/dev/null; then
  # shellcheck disable=SC2034
  PYTHON_VERSION=3.9
else
  echo "No compatible Python was found." >&2
  exit 1
fi

# shellcheck disable=SC1091
source .cfg.sh

set -euf -o pipefail

PYTHON_VENV_DIR="$HOME/.python_venv/main_$PYTHON_VERSION"

ID=$(osid)
PYTHON_VENV_DIR_MAIN_LINK="$HOME/.python_venv/main_$ID"
PYTHON_VENV_DIR="$HOME/.python_venv/${ID}_python$PYTHON_VERSION"

REQUIREMENTS_LOG="$PYTHON_VENV_DIR/requirements.txt"
REQUIREMENTS_FREEZE_LOG="$PYTHON_VENV_DIR/.requirements.freeze"
VERSION_LOG="$PYTHON_VENV_DIR/version.txt"
ATEXIT_DONE=0

rm -f "$REQUIREMENTS_LOG"

atexit() {
  local errno="$?"
  trap - EXIT INT TERM
  if [[ "$ATEXIT_DONE" -eq 0 ]]; then
    if [[ -f "$REQUIREMENTS_LOG" ]]; then
      sort <"$REQUIREMENTS_LOG" | uniq >"$REQUIREMENTS_LOG.new"
      mv "$REQUIREMENTS_LOG.new" "$REQUIREMENTS_LOG"
      rm -f "$REQUIREMENTS_LOG.new"

      python --version >"$VERSION_LOG"
    fi

    ATEXIT_DONE=1
  fi

  if [[ $errno -ne 0 ]]; then
    echo "Error." >&2
  fi
  exit "$errno"
}

init() {
  trap 'atexit' INT TERM EXIT

  if ! [[ -f "$PYTHON_VENV_DIR/bin/activate" ]]; then
    rm -fr "$PYTHON_VENV_DIR"
  fi

  if ! [[ -d "$PYTHON_VENV_DIR" ]]; then
    echo "Python venv created: $PYTHON_VENV_DIR"
    "python${PYTHON_VERSION}" -m venv "$PYTHON_VENV_DIR"
  fi

  echo "Python venv: $PYTHON_VENV_DIR"
  set +e
  # shellcheck disable=SC1091
  source "$PYTHON_VENV_DIR/bin/activate"
  set -e

  pip install --upgrade pip

  echo
  echo "Success."
}

pip_exists() {
  local pip_pkg="$1"
  (grep -v ' @ ' <"$REQUIREMENTS_FREEZE_LOG" \
    | sed -e 's/\(==.*\| \+.*\)$//g' \
    | grep --line-regexp --fixed-strings "$pip_pkg") || return 1
  return 0
}

install_pip_packages() {
  # Install requirements
  local pip_pkg

  echo >"$REQUIREMENTS_LOG"

  readarray -t requirements < <(find work_cfg/tools -name requirements.txt \
    -exec cat {} \; | uniq)
  if [[ ${#requirements[@]} -gt 0 ]]; then
    for pip_pkg in "${requirements[@]}"; do
      echo "$pip_pkg" >>"$REQUIREMENTS_LOG"
    done
  fi

  for pip_pkg in "${PIP_PACKAGES[@]}"; do
    echo "$pip_pkg" >>"$REQUIREMENTS_LOG"
  done

  pip freeze >"$REQUIREMENTS_FREEZE_LOG"

  # Uninstall
  for pip_pkg in "${REMOVE_PIP_PACKAGES[@]}"; do
    if ! pip_exists "$pip_pkg"; then
      continue
    fi

    echo "[RUN] pip uninstall " "$pip_pkg"
    pip uninstall -y "$pip_pkg"
  done

  readarray -t PIP_PACKAGES < <(cat <"$REQUIREMENTS_LOG" | uniq)
  if [[ ${#PIP_PACKAGES[@]} -gt 0 ]]; then
    for pip_pkg in "${PIP_PACKAGES[@]}"; do
      if [[ $pip_pkg = '' ]]; then
        continue
      fi

      echo "[RUN] pip install --upgrade" "$pip_pkg"
      pip install --upgrade "$pip_pkg"

      pip freeze >"$REQUIREMENTS_FREEZE_LOG"
    done
  fi

  if ! [[ -L "$PYTHON_VENV_DIR_MAIN_LINK" ]]; then
    ln -sf "$PYTHON_VENV_DIR" "$PYTHON_VENV_DIR_MAIN_LINK"
  fi
}

main() {
  init

  install_pip_packages
}

main "$@"
atexit
