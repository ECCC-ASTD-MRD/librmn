#!/bin/sh

OSNAME=
OSVERSION=
OS=

# remove white spaces
sansespace() { echo $* | sed 's/ //g'; }

EC_BASE_ARCH=`echo $BASE_ARCH`

# use local variable at EC
if [ -n "${EC_ARCH}" ];
then
    OS="${EC_ARCH}";

fi
printf "${OS}"
