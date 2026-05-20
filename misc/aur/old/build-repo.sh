#!/usr/bin/env bash
# Required vars: GPG_KEY

set -euf -o pipefail

cd "$DEST_REPO_DIR"

# Sign packages
readarray -t LIST_PKG < <(find . -maxdepth 1 -type f -name '*.pkg.tar.zst')
if ! [[ ${#LIST_PKG[@]} -gt 0 ]]; then
    exit 1
fi

for PKG_NAME in "${LIST_PKG[@]}"; do
    [[ -f "$PKG_NAME.sig" ]] && continue
    echo "[GPG SIGN] $PKG_NAME"
    gpg --default-key "$PACKAGES_GPG_KEY" --output "$PKG_NAME.sig" \
        --detach-sign "$PKG_NAME"
done

rm -f custom* .tmp*
repo-add --verify --sign \
    --key "$PACKAGES_GPG_KEY" custom.db.tar.gz "${LIST_PKG[@]}"
