#!/bin/sh
zfs send zones/$1@$2 | gzip
