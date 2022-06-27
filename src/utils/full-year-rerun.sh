#!/usr/bin/env bash

set -o errexit
set -o nounset

src="$1";
dest="$2";

ls "$src" | while read d; do
    if find "${src}/${d}" -mindepth 1 -type d | grep . > /dev/null; then
        echo; echo skipping $d because it has subdirs; echo;
    else
        echo; echo splitting $d; echo;
        mkdir -p "${dest}/${d}";
        $(dirname $0)/split-large-album.sh "${src}/${d}" "${dest}/${d}";
    fi
done
