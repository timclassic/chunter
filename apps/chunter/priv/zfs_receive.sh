#!/bin/sh
bunzip2 | zfs receive zones/$1-partial
zfs rename zones/$1-partial zones/$1

