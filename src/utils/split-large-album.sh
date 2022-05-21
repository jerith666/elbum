#!/usr/bin/env bash

set -o errexit;
set -o nounset;

src=$1;
dest=$2;

(
    #clear out dest
    cd "$dest";
    rm -rvf *;

    #set up subdirs
    cd "$src";
    mkdir left right;
    n=$(ls | wc -l);
    half=$(( n / 2 ));

    #move existing thumbnail out of the way
    if [ -f thumbnail ]; then
        mv -iv thumbnail thumbnail.orig;
    fi

    #move files
    ls | head -n $half | while read leftFile; do
        if [ "$leftFile" != "left" ]; then
            mv -iv "$leftFile" left;
        fi
    done
    ls | while read rightFile; do
        if [ "$rightFile" != "right" ]; then
            if [ "$rightFile" != "left" ]; then
                mv -iv "$rightFile" right;
            fi
        fi
    done

    #make temp thumbnail links
    for d in left right; do
        (
            cd $d;
            ln -s "$(ls | head -1)" thumbnail;
        )
    done

    if [ -L left/thumbnail.orig ]; then
        if [ -e $(readlink left/thumbnail.orig) ]; then
            true;
        else
            mv -iv left/thumbnail.orig right/;
        fi
    fi
    if [ -L right/thumbnail.orig ]; then
        if [ -e $(readlink right/thumbnail.orig) ]; then
            true;
        else
            mv -iv right/thumbnail.orig left/;
        fi
    fi
    
    #generate subdivided album
    elbum "$src" "$dest" || ( echo; echo album generation failed, check for errors above; echo );

    #put things back
    rm left/thumbnail right/thumbnail;
    mv -iv left/* .;
    mv -iv right/* .;
    rmdir left right;
    if [ -f thumbnail.orig ]; then
        mv -iv thumbnail.orig thumbnail;
    fi

    #move results up
    cd "$dest";
    mv -v left/* .;
    rmdir left;
    mv -v right/* .;
    rmdir right;
    rm album.json;
)
