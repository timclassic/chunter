#!/usr/bin/bash

. /usbkey/config

TESTED_VERSIONS=joyent_20120906T221231Z\|joyent_20121203T193049Z\|joyent_20120726T184637Z\|joyent_20121018T224723Z\|joyent_20130222T000747Z\|joyent_20130405T010449Z\|joyent_20130530T224720Z\|joyent_20130419T073558Z

DST=/opt

#IFACE=`dladm show-phys -m | grep $admin_nic | awk '{print $1}'`
#IP=`ifconfig $IFACE | grep inet | awk '{print $2}'`

DIR=`dirname $0`;
if [[ "x$DIR" = "x." ]]
then
    DIR=`pwd`
fi
BASE=`basename $0`;

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
#sed -i .bak -e "s/127.0.0.1/${IP}/g" /opt/chunter/etc/app.config
#sed -i .bak -e "s/127.0.0.1/${IP}/g" /opt/chunter/etc/vm.args

if [ ! -f /opt/chunter/etc/app.config ]
then
    cp /opt/chunter/etc/app.config.example /opt/chunter/etc/app.config
fi

if [ ! -f /opt/chunter/etc/vm.args ]
then
    cp /opt/chunter/etc/vm.args.example /opt/chunter/etc/vm.args
fi

mkdir -p /opt/custom/smf
cp /opt/chunter/share/epmd.xml /opt/chunter/share/chunter.xml /opt/custom/smf

svccfg import /opt/custom/smf/epmd.xml
svccfg import /opt/custom/smf/chunter.xml

cat <<EOF

EOF

exit 0;
