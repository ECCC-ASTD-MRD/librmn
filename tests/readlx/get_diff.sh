#!/usr/bin/env bash

ptr_expr='[0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F][0-9A-F]'

test $# -lt 2 && exit 1

# Remove pointers, since they change at every execution
# Remove warnings, since they can depend on the environment
diff -B -b <(sed -e 's/\s*'${ptr_expr}'\s*/ /' -e 's/^(WARNING) .*$//' < ${1}) <(sed -e 's/\s*'${ptr_expr}'\s*/ /' -e 's/^(WARNING) .*$//' < ${2})
