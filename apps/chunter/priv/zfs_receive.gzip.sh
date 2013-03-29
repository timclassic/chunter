#!/bin/sh
gunzip | zfs receive zones/$1
