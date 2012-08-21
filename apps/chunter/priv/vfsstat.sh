#!/bin/sh 
(cat && kill 0) | vfsstat -M -Z -r 1
