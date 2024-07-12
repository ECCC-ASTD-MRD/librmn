#!/usr/bin/env bash

ptr_expr='[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]'

test $# -lt 2 && exit 1

diff -B -b <(sed -e 's/\s*'${ptr_expr}'\s*/ /' < ${1}) <(sed -e 's/\s*'${ptr_expr}'\s*/ /' < ${2})
