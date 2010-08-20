#! /bin/sh

INSTALLPATH=$1
ROOT="`dirname "$0"`"

if [ $# -ne 1 ]; then
  echo "Usage install_server.sh Path" 
else
  mkdir $1
  mkdir $1/src
  mkdir $1/deps
  mkdir $1/deps/exmpp-0.9.3

  cp -r $ROOT/scripts $INSTALLPATH
  cp -r $ROOT/database $INSTALLPATH
  cp -r $ROOT/data $INSTALLPATH
  cp -r $ROOT/ebin $INSTALLPATH
  cp -r $ROOT/etc $INSTALLPATH
  cp -r $ROOT/log $INSTALLPATH
  cp -r $ROOT/priv $INSTALLPATH

  cp -r $ROOT/src/views $INSTALLPATH/src
  cp -r $ROOT/deps/exmpp-0.9.3/c_src $INSTALLPATH/deps/exmpp-0.9.3
  cp -r $ROOT/deps/exmpp-0.9.3/ebin $INSTALLPATH/deps/exmpp-0.9.3
  rm -rfv $INSTALLPATH/deps/exmpp-0.9.3/c_src/*.c
  rm -rfv $INSTALLPATH/deps/exmpp-0.9.3/c_src/*.h
  rm -rfv $INSTALLPATH/deps/exmpp-0.9.3/c_src/Makefile
  rm -rfv $INSTALLPATH/deps/exmpp-0.9.3/c_src/Makefile.*
fi
