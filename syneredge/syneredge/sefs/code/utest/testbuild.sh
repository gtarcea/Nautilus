#!/bin/sh -x

PATH=$PATH:$SYGHOME/code/utest
export PATH

rm -rf syneredge/sefs/code/clients
rm -rf syneredge/sefs/code/make
rm -rf syneredge/sefs/code/makefile
rm -rf syneredge/sefs/code/policy
rm -rf syneredge/sefs/code/storprovider
rm -rf syneredge/sefs/code/storserv
rm -rf syneredge/sefs/code/utest
rm -rf syneredge/sefs/code/utils
rm -rf syneredge/sefs/code/vbi
rm -rf syneredge/sefs/build/$SYGVER/obj/debug_32/*
rm -rf syneredge/sefs/build/$SYGVER/bin/debug_32/*
rm -rf syneredge/sefs/build/$SYGVER/lib/debug_32/*
cd syneredge/sefs
cvs update -Pd
cd code
make clean
../dolibs
rm /home/developer/make.out /home/developer/tests.out
make > /home/developer/make.out
syg_unittest > /home/developer/tests.out
runtests >> /home/developer/tests.out
