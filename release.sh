# Zips files for binary distribution
BLDDT=$(strings FreeShip |grep Build:|cut -f2 -d' ')
VERS=$(Utils/versinfo FreeShip|grep 'File Full Version:'|cut -f2)
ARCH=$(file -b FreeShip|cut -f6 -d' '|sed 's/,//')

git tag -a v$VERS -m "Test Release. $ARCH Linux Qt. ver $VERS"

./zipbin.sh


