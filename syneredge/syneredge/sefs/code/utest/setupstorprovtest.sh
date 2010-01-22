#!/bin/sh

SPCONFIG=/etc/syneredge/storprov.config

if [ $1 = "START" ] ; then
	rm -f /tmp/sputbf*
	test -f $SPCONFIG && mv $SPCONFIG ${SPCONFIG}.bak
	echo "localhost" > $SPCONFIG
	echo "/tmp/sputbf" >> $SPCONFIG
	echo "1" >> $SPCONFIG
	echo "100" >> $SPCONFIG
	echo "1024" >> $SPCONFIG
else
	rm -f $SPCONFIG
	test -f ${SPCONFIG}.bak && mv ${SPCONFIG}.bak $SPCONFIG
	rm -f /tmp/sputbf*
fi
