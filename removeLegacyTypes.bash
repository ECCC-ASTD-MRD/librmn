#!/bin/bash


function printUsage() {
    echo -e "Replace legacy types in C source code\n"
    echo -e "Usage:"
    echo -e "\t${0} [--dry-run]"
}

dryRun=0

if (( $# >= 1 )); then
    if [[ "$1" == "--dry-run" ]]; then
        dryRun=1
    else
        if [ "$1" == "-h" -o "$1" == "--help" ]; then
            printUsage
            exit
        else
            echo -e "Unknown parameter: ${1}!\n"
            exit 1
        fi
    fi
fi


for filePath in $(find src -name '*.h' -o -name '*.c'); do
    newType=int32_t
    for dataType in ftnword wordint; do
        echo "Processing $filePath Replacing $dataType ..."
        if (( dryRun )); then
            grep --color=always -HnP "(^|\W)${dataType}(\W)" $filePath
        else
            perl -p -i -e "s/(^|\W)${dataType}(\W)/\1${newType}\2/g" $filePath
            perl -p -i -e "s/unsigned\s+${newType}/u${newType}/g" $filePath
        fi
    done

    newType=uint32_t
    dataType=word
    echo "Processing $filePath Replacing $dataType ..."
    if (( dryRun )); then
        grep --color=always -HnP "(^|\W)${dataType}(\W)" $filePath
    else
        perl -p -i -e "s/(^|\W)${dataType}(\W)/\1${newType}\2/g" $filePath
        perl -p -i -e "s/unsigned\s+${newType}/u${newType}/g" $filePath
    fi

    newType=float
    for dataType in ftnfloat wordfloat; do
        echo "Processing $filePath Replacing $dataType ..."
        if (( dryRun )); then
            grep --color=always -HnP "(^|\W)${dataType}(\W)" $filePath
        else
            perl -p -i -e "s/(^|\W)${dataType}(\W)/\1${newType}\2/g" $filePath
        fi
    done
done
