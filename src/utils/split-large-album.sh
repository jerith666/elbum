#!/usr/bin/env bash

set -o errexit;
set -o nounset;

src="$1";
dest="$2";
keepthumbnail=${3:-true};

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
        if [ "${keepthumbnail}" == "true" ]; then
            mv -iv thumbnail thumbnail.orig;
        else
            rm thumbnail
        fi
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

    #see if we're small enough yet
    n=$(ls left | wc -l);
    if [ $n -lt 30 ]; then
        # ensure thumbnail.orig is in the correct right or left subdir
        if [ -L left/thumbnail.orig ]; then
            if [ -e left/$(readlink left/thumbnail.orig) ]; then
                true;
            else
                mv -iv left/thumbnail.orig right/;
            fi
        fi
        if [ -L right/thumbnail.orig ]; then
            if [ -e right/$(readlink right/thumbnail.orig) ]; then
                true;
            else
                mv -iv right/thumbnail.orig left/;
            fi
        fi
    
        #generate subdivided album
        elbum "$src" "$dest" || ( echo; echo album generation failed, check for errors above; echo );
        rm -vf "$dest"/elbum "$dest"/elbum.js "$dest"/index.html "$dest"/.htaccess "$dest"/album.json;
    else
        # move thumbnail.orig out of the way since recursion will want to create it
        if [ -L left/thumbnail.orig ]; then
            mv -iv left/thumbnail.orig .;
        fi
        if [ -L right/thumbnail.orig ]; then
            mv -iv right/thumbnail.orig .;
        fi

        mkdir "$dest/left" "$dest/right";
        echo; echo "recursing $src/left";
        $0 "$src/left" "$dest/left" "false";
        echo; echo "recursing $src/right";
        $0 "$src/right" "$dest/right" "false";
        echo; echo "done recursing for $src";
    fi

    #put things back
    rm -f left/thumbnail right/thumbnail;
    mv -iv left/* .;
    mv -iv right/* .;
    echo "about to remove left and right in $(pwd)"
    ls -a left/
    ls -a right/
    rmdir left right;
    if [ -f thumbnail.orig ]; then
        mv -iv thumbnail.orig thumbnail;
    fi

    #move results up
    cd "$dest";
    mv -v left/* .;
    echo "about to remove left in $(pwd)"
    ls -a left/
    rmdir left;
    mv -v right/* .;
    echo "about to remove right in $(pwd)"
    ls -a right/
    rmdir right;
    rm -vf album.json;
)
