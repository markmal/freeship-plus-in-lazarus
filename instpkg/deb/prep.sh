#!/bin/bash

PRJ_HOME=~/MyProjects/freeship-plus-in-lazarus

#clean
rm -rf freeship/etc
rm -rf freeship/usr

umask 0022

mkdir -p freeship/usr/bin
cp $PRJ_HOME/FreeShip freeship/usr/bin/

mkdir -p freeship/usr/lib
cp $PRJ_HOME/lib/lib*.so freeship/usr/lib/

mkdir -p freeship/etc/FreeShip
cp -r $PRJ_HOME/install/FreeShip.ini freeship/etc/FreeShip/

mkdir -p freeship/usr/share/FreeShip/Import
cp -r $PRJ_HOME/Exec freeship/usr/share/FreeShip/
cp -r $PRJ_HOME/install freeship/usr/share/FreeShip/
cp -r $PRJ_HOME/Languages freeship/usr/share/FreeShip/
cp -r $PRJ_HOME/locale freeship/usr/share/FreeShip/
cp -r $PRJ_HOME/Manuals freeship/usr/share/FreeShip/
cp -r $PRJ_HOME/Ships freeship/usr/share/FreeShip/
cp -r $PRJ_HOME/Themes freeship/usr/share/FreeShip/

mkdir -p freeship/usr/share/applications
mkdir -p freeship/usr/share/doc/freeship
#mkdir -p freeship/usr/share/icons  
mkdir -p freeship/usr/share/menu/freeship
mkdir -p freeship/usr/share/pixmaps
mkdir -p freeship/usr/share/man/man1/

cp $PRJ_HOME/Whatsnew.txt freeship/usr/share/doc/freeship/changelog
gzip -n -9 freeship/usr/share/doc/freeship/changelog
grep -v '^# ' $PRJ_HOME/copyright > freeship/usr/share/doc/freeship/copyright
cp $PRJ_HOME/install/menu freeship/usr/share/menu/freeship/menu
convert $PRJ_HOME/Themes/Default/icons/32/00-freeship.png freeship/usr/share/pixmaps/freeship.xpm
cp $PRJ_HOME/install/FreeShip.1.gz freeship/usr/share/man/man1/

# remove some stuff
rm freeship/usr/share/FreeShip/Themes/Default/icons/mkicons-from-svg.sh
rm freeship/usr/share/FreeShip/Themes/Default/icons/make_freeship_menu_icon_sprite.sh
rm -rf freeship/usr/share/FreeShip/Themes/Default/icons/preview

VER=$($PRJ_HOME/Utils/versinfo $PRJ_HOME/FreeShip |grep 'File Full Version:'|cut -f 2)
ISZ=$(du -ks freeship | cut -f 1)
#change Version and Installed Size in control file
sed \
 -e "s/^Version: .*/Version: $VER/" \
 -e "s/^Installed-Size: .*/Installed-Size: $ISZ/" \
 freeship/DEBIAN/control > control.tmp
cp control.tmp freeship/DEBIAN/control

