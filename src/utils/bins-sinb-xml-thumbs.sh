#!/usr/bin/env bash

set -o errexit;
set -o nounset;

find . -name album.xml | while read a; do
    t=$(xmllint --xpath "/album/description/field[@name='sampleimage']/text()" "$a" | sed 's/^[[:space:]]*//g' | tr -d '\n');
    (cd "$(dirname "$a")";
     pwd;
     ln -sf "${t}" thumbnail;)
done
