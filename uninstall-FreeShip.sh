#!/bin/bash
echo "Uninstallation FreeShip"

FS_HOME=$(cd $(dirname $0) && pwd)

if [ -f "${FS_HOME}/install/uninstall.var" ]; then
  source "${FS_HOME}/install/uninstall.var"
else
  echo "${FS_HOME}/install/uninstall.var" not found
  exit 1
fi

if [ "$INST_SCOPE" == "U" ]; then
  FS_GCONF="${FS_HOME}"
  XDG_MODE=user
fi

if [ "$INST_SCOPE" == "M" ]; then
  if [ "$(whoami)" != "root" ]; then
    echo "Please run this uninstallation script as root or using 'sudo'"
    exit 1
  fi
  FS_GCONF="/etc/FreeShip"
  XDG_MODE=system
fi

CONFIRM="N"
while [ "$CONFIRM" == "N" ]
do
  read -e -p "Uninstall FreeShip from '${FS_HOME}'
Following will be deleted:
 - all unmodified files from this directory.
 - '$FS_BIN/FreeShip'
 - menu items and icons

Y - Uninstall
Q - quit
[Y|Q]: " CONFIRM
 case $CONFIRM in
  Y) 
    CONFIRM=Y
    ;;
  y) 
    echo Type capital Y
    CONFIRM=N
    ;;
  Q|q) 
    echo Thanks. Bye.
    exit
    ;;
  *)
    CONFIRM=N
    ;;
 esac
done

cd "$FS_HOME/install/"

if [ -x "$(which xdg-desktop-menu 2>/dev/null)" ]; then
    echo "  Uninstall Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu uninstall --novendor --mode "$XDG_MODE" Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu possibly not uninstalled. xdg-desktop-menu not found."
fi

if [ -x "$(which xdg-mime 2>/dev/null)" ]; then
    echo "  Uninstall MIME"
    xdg-mime uninstall --mode "$XDG_MODE" application.freeship-model-fbm.xml
    xdg-mime uninstall --mode "$XDG_MODE" application.freeship-model-ftm.xml
else
 echo "Warning: MIME possibly not uninstalled. xdg-mime not found."
fi

if [ -x "`which xdg-icon-resource 2>/dev/null`" ]; then
    echo "  Uninstall Icons"
    for SZ in 16 24 32 48 64 96 128; do
      xdg-icon-resource uninstall --novendor --context apps --mode "$XDG_MODE" --size ${SZ} freeship
      xdg-icon-resource uninstall --novendor --context mimetypes --mode "$XDG_MODE" --size ${SZ} application-freeship-model-fbm
      xdg-icon-resource uninstall --novendor --context mimetypes --mode "$XDG_MODE" --size ${SZ} application-freeship-model-ftm
    done
    xdg-icon-resource forceupdate
else
 echo "Warning: Icons possibly not uninstalled. xdg-icon-resource not found."
fi

if [ -x "$(which update-desktop-database 2>/dev/null)" ]; then
    echo "  Update Desktop database"
    update-desktop-database "${SHARE_DIR}/applications"
else
 echo "Warning: Desktop database not updated. update-desktop-database not found."
fi

if [ -x "$(which update-mime-database 2>/dev/null)" ]; then
    echo "  Update MIME database"
    update-mime-database "${SHARE_DIR}/mime"
else
 echo "Warning: MIME database not updated. update-mime-database not found."
fi

cd "${FS_HOME}"

chmod -R u+w "${FS_HOME}"

# delete files that match checksum
cd "${FS_HOME}"
while read MD5 FN; do 
  if [ -f "$FN" ] ; then
    SM=$(md5sum "$FN")
    SM=${SM:0:32}
    if [ "$SM" == "$MD5" ]; then
      rm -f "$FN"
    else
      echo "Keep modified '$FN'"
    fi
  fi  
done < install/.original-files.md5

# delete empty dirs
(sort -r install/.original-dirs.lst | grep -v '^\.\/install$') \
| while read D; do [ -d "$D" ] && rmdir "$D"; done

rm -f install/.original-dirs.lst install/.original-files.md5
rm -f install/freeship.desktop install/uninstall.var

cd "${FS_HOME}"

[ -d "${FS_HOME}/Import" ] && rmdir "${FS_HOME}/Import"
[ -d "${FS_HOME}/install" ] && rmdir "${FS_HOME}/install"

cd "${FS_HOME}"
rm -f COPYING copyright install-HOWTO.txt README.txt uninstall-FreeShip.sh

if [ "$INST_SCOPE" == "M" ]; then
  [ -d '/etc/FreeShip' ] && rmdir '/etc/FreeShip'
fi

[ -x "${FS_BIN}/FreeShip" ] && rm -f "${FS_BIN}/FreeShip"

rmdir "${FS_HOME}"

echo "Done"
