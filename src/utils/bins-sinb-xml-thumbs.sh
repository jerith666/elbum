#!/usr/bin/env bash

set -o errexit;
set -o nounset;

find . -name album.xml | sort | while read a; do
    t=$(xmllint --xpath "/album/description/field[@name='sampleimage']/text()" "$a" | sed 's/^[[:space:]]*//g' | tr -d '\n');
    if [ -z "${t}" ]; then
        t="$(find "${a/album.xml/}" -iname *.jpg -or -iname *.jpeg -or -iname *.png | head -1)";
        t=$(basename "$t");
        echo "falling back to default thumbnail $t";
    fi
    
    (cd "$(dirname "$a")";
     pwd;
     ln -sf "${t}" thumbnail;)
done
