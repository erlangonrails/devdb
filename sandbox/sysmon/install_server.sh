#! /bin/sh

INSTALLPATH=$1
ROOT="`dirname "$0"`"

if [ $# -ne 1 ]; then
  echo "Usage install_server.sh Path" 
else
  mkdir $1
  mkdir $1/src

  cp -r $ROOT/scripts $INSTALLPATH
  cp -r $ROOT/database $INSTALLPATH
  cp -r $ROOT/data $INSTALLPATH
  cp -r $ROOT/ebin $INSTALLPATH
  cp -r $ROOT/etc $INSTALLPATH
  cp -r $ROOT/log $INSTALLPATH
  cp -r $ROOT/priv $INSTALLPATH

  cp -r $ROOT/src/views $INSTALLPATH/src
fi
