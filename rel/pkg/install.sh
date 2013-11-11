#!/usr/bin/bash

. /usbkey/config

TESTED_VERSIONS=20130530T224720Z\|20130419T073558Z\|20130629T040542Z\|20130808T195337Z\|20131003T221245Z

if [ -z $DST ]
then
    DST=/opt
fi

#IFACE=`dladm show-phys -m | grep $admin_nic | awk '{print $1}'`
#IP=`ifconfig $IFACE | grep inet | awk '{print $2}'`

DIR=`dirname $0`;
if [[ "x$DIR" = "x." ]]
then
    DIR=`pwd`
fi
BASE=`basename $0`;


if uname -a | egrep 20130627T201726Z
then
    echo "Sorry this SmartOS version is known to be incompatible or faulty."
    exit 1
elif uname -a | egrep 20131031T235904Z
then
    echo "Sorry this SmartOS version is known to be incompatible or faulty."
    echo " 20131031T235904Z: missing mdata-get ( http://bit.ly/Hyzb1e )"
    exit 1
fi


if uname -a | egrep $TESTED_VERSIONS
then
    echo "This SmartOS release is tested!"
else
    echo "This SmartOS release WAS NOT tested! Are you sure you want to go on? [yes|NO] "
    read SKIP
    if [[ "$SKIP" = "yes" ]]
    then
	echo "Okay we go on, but it might not work!"
    else
	echo "Exiting."
	exit 1
    fi
fi

# We've to initialize imgadm or it will die horribly .... *sigh*
[ -d /var/imgadm ] || imgadm update
[ -d /var/imgadm/images ] || mkdir -p /var/imgadm/images

(cd $DST; uudecode -p $DIR/$BASE|tar xzf -)
mkdir -p /var/log/chunter

if [ ! -f $DST/chunter/etc/app.config ]
then
    cp $DST/chunter/etc/app.config.example $DST/chunter/etc/app.config
fi

if [ ! -f $DST/chunter/etc/vm.args ]
then
    cp $DST/chunter/etc/vm.args.example $DST/chunter/etc/vm.args
fi

mkdir -p $DST/custom/smf
cp $DST/chunter/share/epmd.xml $DST/custom/smf
cp $DST/chunter/share/chunter.xml $DST/custom/smf

svccfg import $DST/custom/smf/epmd.xml
svccfg import $DST/custom/smf/chunter.xml

exit 0;
