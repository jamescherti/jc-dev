#!/usr/bin/env bash
# Parameters
# ----------
# DEST_REPO_DIR=/path/http/archlinux
# PACKAGES_GPG_KEY=user@domain.com
# GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES=/opt/archlinux-packages
# DOCKER_USER_GID=$(id -g)
# DOCKER_USER_UID=$(id -u)

set -eu -o pipefail

if [ "$(id -u)" -ne "0" ]; then
  echo "Error: you need root privileges to run this script." >&2
  exit 1
fi

cd "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES"

set -o xtrace

#----------------------------------------------------------------
# INIT
#----------------------------------------------------------------
pacman -Syu --noconfirm
groupadd --gid "$DOCKER_USER_GID" docker_user
useradd -m -g docker_user --uid "$DOCKER_USER_UID" docker_user
echo 'docker_user ALL=(ALL) NOPASSWD: ALL' >/etc/sudoers.d/docker-user

#----------------------------------------------------------------
# INIT docker_user
#----------------------------------------------------------------
cd "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES"
sudo -i -u docker_user gpg \
  --import "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES/repo.gpg.asc"

#----------------------------------------------------------------
# Build
#----------------------------------------------------------------
build_package() {
  cd "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES/$1"
  if ! sudo -H -u docker_user makepkg -f --noconfirm --syncdeps; then
    rm -f *.pkg.*
    return 1
  fi
}

build_package_once() {
  cd "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES/$1"
  amount_pkg=$(find . -maxdepth 1 -name '*.pkg.*' | wc -l)
  if [[ $amount_pkg -eq 0 ]]; then
    build_package "$1"
  fi
}

# TODO add other packages

# PACKAGE: NVIDIA
# cd "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES/nvidia-470xx-utils"
# rm -fr src/NVIDIA-Linux-x86_64-470.182.03
# build_package nvidia-470xx-utils
# # nvidia-470xx-settings dependency
# pacman --noconfirm -U nvidia-470xx-utils*.pkg*.tar*
# build_package nvidia-470xx-settings

# PACKAGE: ANANICY
# build_package_once ananicy

#----------------------------------------------------------------
# REPO
#----------------------------------------------------------------
cd "$GIT_LOCAL_CLONE_ARCHLINUX_PACKAGES"
sudo -H -u docker_user mkdir -p "$DEST_REPO_DIR"
sudo -H -u docker_user \
  find . -type f -name '*.pkg.tar.zst' -not -path "$DEST_REPO_DIR/*" \
  -exec cp -v '{}' "$DEST_REPO_DIR" ';'

sudo --preserve-env=PACKAGES_GPG_KEY,DEST_REPO_DIR -H \
  -u docker_user ./build-repo.sh
