#!/bin/sh
echo "`date +'%Y-%m-%d %H:%M:%S'` Receiving snapshot $2 for $1 encoded as gzip" >> /var/log/chunter/zfs.log
echo "running: gunzip | zfs receive $1@$2 >> /var/log/chunter/zfs_receive.log 2>> /var/log/chunter/zfs.log"
gunzip | zfs receive $1@$2 >> /var/log/chunter/zfs.log 2>> /var/log/chunter/zfs.log
