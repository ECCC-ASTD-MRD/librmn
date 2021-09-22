#!/bin/bash


filePath=src/fstd98/xdf98.c
#for filePath in $(find src -name '*.h' -o -name '*.c'); do
    newType=int32_t
    for dataType in ftnword wordint word; do
        echo "Processing $filePath Replacing $dataType ..."
#         grep --color=always -HnP "(^|\W)${dataType}(\W)" $filePath
        perl -p -i -e "s/(^|\W)${dataType}(\W)/\1${newType}\2/g" $filePath
        perl -p -i -e "s/unsigned\s+${newType}/u${newType}/g" $filePath
    done

#     newType=float
#     for dataType in ftnfloat wordfloat; do
# #         echo "Processing $filePath Replacing $dataType ..."
#         grep --color=always -HnP "(^|\W)${dataType}(\W)" $filePath
# #         perl -p -i -e "s/(^|\W)${dataType}(\W)/\1${newType}\2/g" $filePath
#     done
# done