#!/bin/sh
OPTS=""
if [ ! -z "$3" ];
then
    OPTS="-i $1@$3"
fi
zfs send $OPTS $1@$2 | gzip -1
