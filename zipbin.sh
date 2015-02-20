# Zips files for binary distribution
BLDDT=$(strings FreeShip |grep Build:|cut -f2 -d' ')
VERS=$(Utils/versinfo FreeShip|grep 'File Full Version:'|cut -f2)
ARCH=$(file -b FreeShip|cut -f6 -d' '|sed 's/,//')
NAME=FreeShip-${VERS}_${ARCH}_linux_gtk2
md5sum FreeShip >FreeShip.md5
zip -r ${NAME}.zip Languages FreeShip FreeShip.md5 Whatsnew.txt
md5sum ${NAME}.zip > ${NAME}.md5
