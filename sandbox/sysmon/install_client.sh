#! /bin/sh

INSTALLPATH=$1
ROOT="`dirname "$0"`"

if [ $# -ne 1 ]; then
  echo "Usage install_client.sh CrontabRoot" 
else
  cp -r $ROOT/ebin/sysmon_client_api.beam $INSTALLPATH/ebin
  cp -r $ROOT/ebin/sysmon_client_util.beam $INSTALLPATH/ebin
  cp -r $ROOT/ebin/monitor_app_task.beam $INSTALLPATH/ebin
fi
