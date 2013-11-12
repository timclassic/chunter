#!/bin/sh
if [ -a /dev/zvol/rdsk/zones/$1-disk0 ]
then
    zfs send zones/$1-disk0@$2 | gzip
else
    zfs send zones/$1@$2 | gzip
fi
