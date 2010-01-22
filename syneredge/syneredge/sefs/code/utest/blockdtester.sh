#!/bin/sh
PATH=$PATH:/filesafe:/filesafe/blockd
export PATH

stopfs
synwrblockdtester
stopfs

if [ $? -ne 0 ] ; then
	echo "synwrblockdtester failed"
	exit 1
fi

DIFFS=`diff /tmp/syneredgeblocks /filesafe/blockd/testblocks`

for i in `ls /filesafe/blockd/testblocks | grep -v CVS`
do
	DIFFS=`diff /tmp/syneredgeblocks/$i /filesafe/blockd/testblocks/$i`

	if [ $? != 0 ] ; then
		echo "blockdtester.sh FAILED $i"
		exit 1
	fi

	if [ "$DIFFS" != "" ] ; then
		echo "blockdtester.sh FAILED $i"
		exit 1
	fi
done

echo "blockdtester.sh SUCCEEDED"
exit 0

