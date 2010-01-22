#!/bin/sh

rm -f /etc/syneredge/nextblocknum
test -f /etc/syneredge/mirrors.bak || cp /etc/syneredge/mirrors /etc/syneredge/mirrors.bak
echo spelljammer /tmp/syneredgeblocks > /etc/syneredge/mirrors
echo buford /tmp/syneredgeblocks2 >> /etc/syneredge/mirrors

