#!/bin/bash

ZIP_HOME=$(pwd)
cd ../../;
PRJ_HOME=$(pwd)

# on Windows it requires Cygwin or MSYS
case ${OSTYPE} in
 msys|cygwin )
   # deprecated. Now using WIX
   versinfo=versinfo.exe
   FreeShip=FreeShip.exe
   INST_SCRIPTS="uninstall-user.cmd install-user.cmd"
   MISCFILES="*.dll"
   ARCH=$(file -b ${FreeShip}|cut -f4 -d' '|sed 's/,//')
   OS=windows
   WidgetSet=win32
   ;;
 linux-gnu )
   versinfo=versinfo
   FreeShip=FreeShip
   LIBS="lib/lib*.so"
   INST_SCRIPTS="install-FreeShip.sh uninstall-FreeShip.sh"
   MISCFILES=""
   ARCH=$(file -b ${FreeShip}|cut -f5 -d' '|sed 's/,//')
   OS=linux
   if (nm FreeShip | grep 'RTTI_$GTK2INT_$$_TGTK2WIDGETSET'); then WidgetSet=gtk2; fi
   if (nm FreeShip | grep 'RTTI_$GTK3INT_$$_TGTK3WIDGETSET'); then WidgetSet=gtk3; fi
   ;;
esac

# Zips files for binary distribution
#BLDDT=$(strings ${FreeShip} |grep Build:|cut -f2 -d' ')
#VERS=$(Utils/$versinfo ${FreeShip}|grep 'File Full Version:'|cut -f2)
VERS=$(Utils/$versinfo --FileFullVersion --NoVerbose ${FreeShip}|tr -d '\r'|tr -d '\n')
NAME=$PRJ_HOME/Releases/FreeShip-${VERS}_${ARCH}_${OS}_${WidgetSet}
md5sum ${FreeShip} >FreeShip.md5

rm ${NAME}.zip ${NAME}.md5 2>/dev/null

zip -9 -r ${NAME}.zip Exec Languages Manuals Ships Themes install locale \
 ${FreeShip} ${LIBS} ${MISCFILES} FreeShip.md5 \
 COPYING copyright README.txt

if [ $? -ne 0 ]; then echo "Error while zip" ; exit 1; fi

cd $ZIP_HOME
zip -9 ${NAME}.zip ${INST_SCRIPTS} install-HOWTO.txt

if [ $? -ne 0 ]; then echo "Error while zip" ; exit 1; fi

md5sum ${NAME}.zip > ${NAME}.md5

if [ $? -ne 0 ]; then echo "Error while md5sum" ; exit 1; fi

echo "Created files:"
echo ${NAME}.zip
echo ${NAME}.md5
