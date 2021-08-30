#!/bin/bash

printUsage() {
    scriptName="$(basename "$(test -L "$0" && readlink "$0" || echo "$0")")"
    echo -e "Extract function prototypes from C source code\n"
    echo -e "Current dir will be used as working directory\n"
    echo -e "Usage:"
    echo -e "\tscriptName srcDirPrefix sourceFilePath [sourceFilePath ...]"
}

if [[ "$1" == "-h" || "$1" == "--help" ]]; then
    printUsage
    exit 0
fi

if (( $# < 2 )); then
    printUsage
    exit 1
fi

srcDirPrefix="$1"

echo -e "// Prototypes automatically extracted from the C source code with ctags\n" > cprotos.h

# FIXME We have functions defined in *.h files!
for cSourceFile in ${@:2}; do
    strippedName="${cSourceFile##$srcDirPrefix/}"
    echo -e "\n\n// ${strippedName}" >> cprotos.h
    ctags -x --_xformat="%{C.properties} %{typeref} %{name}%{signature};" --sort=foldcase --kinds-C=f "${cSourceFile}" | sed -E -e 's/typename://g' -e 's/^[[:space:]]+//g' | grep -vP '(F77|f77|^static)' >> cprotos.h
done
