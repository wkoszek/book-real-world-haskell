#!/bin/sh
#set -x
export ROOT=`pwd`
find * -name "*.ghci" | while read GHCI; do
	BASEDIR=`dirname $GHCI`
	BASENAME=`basename $GHCI`

	echo "#SRC $GHCI ----------------------------------------------------"
	cd $BASEDIR
	ghci < $BASENAME
	ret=$?
	echo "#RUN $ret SRC $GHCI"
	sleep 1
	cd ${ROOT}
done
