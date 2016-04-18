# on Windows it requires Cygwin or MSYS
case ${OSTYPE} in
 msys|cygwin )
   versinfo=versinfo.exe
   FreeShip=FreeShip.exe
   INST_SCRIPTS="uninstall-user.cmd install-user.cmd"
   MISCFILES="*.dll"
   OS=windows;;
 linux-gnu )
   versinfo=versinfo
   FreeShip=FreeShip
   INST_SCRIPTS="install-system.sh uninstall-system.sh uninstall-user.sh install-user.sh"
   MISCFILES=""
   OS=linux;;
esac

# Zips files for binary distribution
BLDDT=$(strings FreeShip |grep Build:|cut -f2 -d' ')
VERS=$(Utils/versinfo --FileFullVersion --NoVerbose ${FreeShip})
ARCH=$(file -b ${FreeShip}|cut -f6 -d' '|sed 's/,//')

if [ -x /usr/bin/git ]
  git tag -a v$VERS -m "Test Release. $ARCH Linux Qt. ver $VERS"
fi

./zipbin.sh

# if we have tools for .deb creation
if [ -x /usr/bin/lintian ] ; then
  # create deb package
  cd instpkg/deb/
  ./prep.sh
  ./build.sh
  cd -
  mv instpkg/deb/freeship_*.deb .
fi