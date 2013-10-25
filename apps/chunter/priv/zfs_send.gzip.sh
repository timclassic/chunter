#!/bin/sh
if zfs list zones/$1-disk0 2&> /dev/null
then
    zfs send zones/$1-disk0@$2 | gzip
else
    zfs send zones/$1@$2 | gzip
fi
