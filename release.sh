# on Windows it requires Cygwin or MSYS
case ${OSTYPE} in
 msys|cygwin )
   versinfo=versinfo.exe
   FreeShip=FreeShip.exe
   INST_SCRIPTS="uninstall-user.cmd install-user.cmd"
   MISCFILES="*.dll"
   ARCH=$(file -b ${FreeShip}|cut -f4 -d' '|sed 's/,//')
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
VERS=$(Utils/versinfo --FileFullVersion --NoVerbose ${FreeShip}|tr -d '\r'|tr -d '\n')

if [ -x /usr/bin/git ]
then
  git tag -a v$VERS -m "Test Release. $ARCH $OS Qt. ver $VERS"
fi

[ $OS = linux ] && ./zipbin.sh

if [ $OS = windows ]; then
  cd instpkg/wix/
  cmd.exe /C make_installer.cmd
  cd -
  mv instpkg/wix/FreeShip_x64.msi ./"FreeShip-${VERS}_x64.msi"
fi

# if we have tools for .deb creation
if [ -x /usr/bin/lintian ] ; then
  echo
  echo Create deb package
  cd instpkg/deb/
  ./prep.sh
  ./build.sh
  cd -
  mv instpkg/deb/freeship_*.deb .
fi
