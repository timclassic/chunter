#!/bin/sh
OPTS=""
if [ -a /dev/zvol/rdsk/zones/$1-disk0 ]
then
    if [ ! -z "$3" ];
    then
        OPTS="-i zones/$1-disk0@$3"
    fi
    zfs send $OPTS zones/$1-disk0@$2 | gzip
else
    if [ ! -z "$3" ];
    then
        OPTS="-i zones/$1@$3"
    fi
    zfs send $OPTS zones/$1@$2 | gzip
fi
