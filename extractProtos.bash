#!/bin/bash

# FIXME We have functions defined in *.h files!
for cSourceFile in $(find src -type f -name "*.c" -a -not -ipath '*test*'); do
    tr '\n' ' ' < ${cSourceFile}  | sed 's/\([{};]\)/\1\n/g' | ctags -x --_xformat="%N %n %F %C %S" --sort=foldcase --kinds-C=f --
done

#ctags -x --_xformat="%-16N %4n %-16F %C %S" --sort=foldcase --kinds-C=f $(find src -type f -name "*.c" | grep -v test) > functions.list
#grep -vP '(f77name|f77_name|CLIB_F77NAME)' functions.list | grep -v static | grep -v extern > functions_filtered.list
#perl -ne 'm/(?:\S+\s+\S+\s+\S+\s+)(.*)/ && print "$1\n"' < functions_filtered.list > functions_filtered.h
#grep --color=auto -E '[[:space:]]+/\*[[:space:]]+%ENTRY%[[:space:]]+\*/[[:space:]]+' functions_filtered.h
#sed -E 's#[[:space:]]+/\*[[:space:]]+%ENTRY%[[:space:]]+\*/[[:space:]]+##g' functions_filtered.h > functions_filtered2.h
#mv functions_filtered2.h functions_filtered.h
