#! /bin/sh

INSTALLPATH=$1
ROOT="`dirname "$0"`"

if [ $# -ne 1 ]; then
  echo "Usage install.sh Path" 
else
  mkdir $INSTALLPATH
  mkdir $INSTALLPATH/bin

  cp -r $ROOT/bin/crontabctl $INSTALLPATH/bin
  cp -r $ROOT/ebin $INSTALLPATH
  cp -r $ROOT/etc $INSTALLPATH
  cp -r $ROOT/log $INSTALLPATH
fi
