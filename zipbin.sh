# Zips files for binary distribution
BLDDT=$(strings FreeShip |grep Build:|cut -f2 -d' ')
VERS=$(Utils/versinfo FreeShip|grep 'File Full Version:'|cut -f2)
ARCH=$(file -b FreeShip|cut -f6 -d' '|sed 's/,//')
NAME=FreeShip-${VERS}_${ARCH}_linux_qt
md5sum FreeShip >FreeShip.md5

rm ${NAME}.zip ${NAME}.md5 2>/dev/null

zip -r ${NAME}.zip Languages Manuals Ships Themes FreeShip FreeShip.md5 Whatsnew.txt \
 install-system.sh uninstall-system.sh uninstall-user.sh install-user.sh \
 Engineering.directory \
 freeship-16.png freeship-24.png freeship-32.png freeship-48.png

if [ $? -ne 0 ]; then echo "Error while zip" ; exit 1; fi

md5sum ${NAME}.zip > ${NAME}.md5

if [ $? -ne 0 ]; then echo "Error while md5sum" ; exit 1; fi

echo "Created files:"
echo ${NAME}.zip
echo ${NAME}.md5
