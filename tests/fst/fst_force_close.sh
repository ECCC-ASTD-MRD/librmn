#!/usr/bin/env bash

GEN_ABORT_FILE=${1}
FST_CHECK=${2}
FST_FORCE_CLOSE=${3}

FILE_RSF="abort.rsf"
FILE_XDF="abort.xdf"

set -x
${GEN_ABORT_FILE} 1
${GEN_ABORT_FILE} 0
set +x

for f in ${FILE_RSF} ${FILE_XDF}; do
    if ${FST_CHECK} ${f}; then
        echo "${f} should not be OK at this point"
        exit -1
    fi

    if ! ${FST_FORCE_CLOSE} ${f}; then
        echo "Unable to force close ${f}"
        exit -1
    fi

    if ! ${FST_CHECK} ${f}; then
        echo "${f} should be OK at this point"
        exit -1
    fi
done

exit 0
