#!/bin/sh
bunzip2 | zfs receive zones/$1
