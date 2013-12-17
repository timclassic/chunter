#!/bin/sh
echo "`date +'%Y-%m-%d %H:%M:%S'` Importing dataset $1 encoded as gzip" >> /var/log/chunter/zfs.log
gunzip | zfs receive zones/$1@$2 >> /var/log/chunter/zfs_receive.log 2>> /var/log/chunter/zfs.log
