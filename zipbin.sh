# on Windows it requires Cygwin or MSYS
case ${OSTYPE} in
 msys|cygwin )
   versinfo=versinfo.exe
   FreeShip=FreeShip.exe
   INST_SCRIPTS="uninstall-user.cmd install-user.cmd"
   MISCFILES="*.dll"
   ARCH=$(file -b ${FreeShip}|cut -f5 -d' '|sed 's/,//')
   OS=windows;;
 linux-gnu )
   versinfo=versinfo
   FreeShip=FreeShip
   INST_SCRIPTS="install-system.sh uninstall-system.sh uninstall-user.sh install-user.sh"
   MISCFILES=""
   ARCH=$(file -b ${FreeShip}|cut -f5 -d' '|sed 's/,//')
   OS=linux;;
esac

# Zips files for binary distribution
#BLDDT=$(strings ${FreeShip} |grep Build:|cut -f2 -d' ')
#VERS=$(Utils/$versinfo ${FreeShip}|grep 'File Full Version:'|cut -f2)
VERS=$(Utils/$versinfo --FileFullVersion --NoVerbose ${FreeShip}|tr -d '\r'|tr -d '\n')
NAME=FreeShip-${VERS}_${ARCH}_${OS}_qt
md5sum ${FreeShip} >FreeShip.md5

rm ${NAME}.zip ${NAME}.md5 2>/dev/null

zip -r ${NAME}.zip Languages Manuals Ships Themes install ${FreeShip} ${MISCFILES} FreeShip.md5 Whatsnew.txt \
 ${INST_SCRIPTS} install-HOWTO.txt \
 "GNU General Public License (GPL).txt" copyright

if [ $? -ne 0 ]; then echo "Error while zip" ; exit 1; fi

md5sum ${NAME}.zip > ${NAME}.md5

if [ $? -ne 0 ]; then echo "Error while md5sum" ; exit 1; fi

echo "Created files:"
echo ${NAME}.zip
echo ${NAME}.md5
