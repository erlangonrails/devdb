#! /bin/sh

APPNAME=erlcrontab
ROOT="`dirname "$0"`"

if [ $# -ne 1 ]; then
  echo "Usage: init.sh install | clean"
else
  case "$1" in
    install)
	 cp $ROOT/bin/$APPNAME /etc/init.d

         ln -s /etc/init.d/$APPNAME /etc/rc2.d/S99$APPNAME
         ln -s /etc/init.d/$APPNAME /etc/rc3.d/S99$APPNAME
         ln -s /etc/init.d/$APPNAME /etc/rc4.d/S99$APPNAME
         ln -s /etc/init.d/$APPNAME /etc/rc5.d/S99$APPNAME
	 ;;
    clean)
         rm /etc/rc2.d/S99$APPNAME
         rm /etc/rc3.d/S99$APPNAME
         rm /etc/rc4.d/S99$APPNAME
         rm /etc/rc5.d/S99$APPNAME

         rm /etc/init.d/$APPNAME
         ;;
      *)
         echo "Usage: init.sh install | clean" 
         ;;
  esac
fi

