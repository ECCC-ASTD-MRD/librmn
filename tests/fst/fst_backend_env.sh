#!/usr/bin/env bash

# Verify that the default backend used when creating a new FST file (i.e. when no
# RSF/XDF is given in the open options) is controlled by the BACKEND option of the
# FST_OPTIONS environment variable, and that the default (no FST_OPTIONS) is RSF.
#
# For each scenario, fst_write_single creates one file with the legacy fst98 API and
# one with the fst24 API; get_backend_name then reports the backend of each file,
# which is compared to the expected value.
#
# Usage: fst_backend_env.sh <fst_write_single> <get_backend_name>

FST_WRITE_SINGLE=${1}
GET_BACKEND_NAME=${2}

FILE_FST98="fst98_single.fst"
FILE_FST24="fst24_single.fst"

# Each entry is "FST_OPTIONS value|expected backend"; an empty value means the
# variable is unset.
scenarios=(
    "|RSF"
    "BACKEND=XDF|XDF"
    "BACKEND=RSF|RSF"
    "BACKEND=xdf|XDF"
    "BACKEND=rsf|RSF"
    "BACKEND=FOO|RSF"
    "BACKEND=|RSF"
    "BACKEND=RS|RSF"
)

for scenario in "${scenarios[@]}"; do
    options="${scenario%%|*}"
    expected="${scenario##*|}"

    if [ -n "${options}" ]; then
        if ! FST_OPTIONS="${options}" "${FST_WRITE_SINGLE}"; then
            echo "FAIL: fst_write_single failed (FST_OPTIONS='${options}')"
            exit 1
        fi
    else
        if ! env -u FST_OPTIONS "${FST_WRITE_SINGLE}"; then
            echo "FAIL: fst_write_single failed (FST_OPTIONS unset)"
            exit 1
        fi
    fi

    for f in "${FILE_FST98}" "${FILE_FST24}"; do
        if ! actual=$("${GET_BACKEND_NAME}" "${f}"); then
            echo "FAIL: get_backend_name failed on ${f} (FST_OPTIONS='${options}')"
            exit 1
        fi
        if [ "${actual}" != "${expected}" ]; then
            echo "FAIL: FST_OPTIONS='${options}' ${f}: expected backend '${expected}', got '${actual}'"
            exit 1
        fi
        echo "OK: FST_OPTIONS='${options}' ${f} -> ${actual}"
    done
done

exit 0
