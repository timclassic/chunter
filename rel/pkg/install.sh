#!/usr/bin/bash

. /usbkey/config

DST=/tmp

IFACE=`dladm show-phys -m | grep $admin_nic | awk -e '{print $1}'`
IP=`ifconfig $IFACE | grep inet | awk -e '{print $2}'`

DIR=`dirname $0`;
if [ x$DIR = "x." ]
then
    DIR=`pwd`
fi
BASE=`basename $0`;

(cd $DST; uudecode -p $DIR/$BASE|tar xzfv -)

sed -i .bak -e "s/127.0.0.1/${IP}/g" /opt/chunter/etc/app.config
sed -i .bak -e "s/127.0.0.1/${IP}/g" /opt/chunter/etc/vm.args

exit 0;
