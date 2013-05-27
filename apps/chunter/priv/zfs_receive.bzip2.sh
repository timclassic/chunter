#!/bin/sh
echo "`date +'%Y-%m-%d %H:%M:%S'` Importing dataset $1 encoded as gzip"
bunzip2 | zfs receive zones/$1 >> /var/log/chunter/zfs_receive.log 2>> /var/log/chunter/zfs.log
