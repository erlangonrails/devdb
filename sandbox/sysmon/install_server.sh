#! /bin/sh

INSTALLPATH=$1
ROOT="`dirname "$0"`"

if [ $# -ne 1 ]; then
  echo "Usage install_server.sh Path" 
else
  cp -r $ROOT/data $INSTALLPATH
  cp -r $ROOT/database $INSTALLPATH
  cp -r $ROOT/ebin $INSTALLPATH
  cp -r $ROOT/etc $INSTALLPATH
  cp -r $ROOT/log $INSTALLPATH
  cp -r $ROOT/scripts $INSTALLPATH
fi
