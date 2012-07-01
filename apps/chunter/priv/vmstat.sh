#!/bin/sh 
sleep 0.5
(cat && kill 0) | vmstat 1
