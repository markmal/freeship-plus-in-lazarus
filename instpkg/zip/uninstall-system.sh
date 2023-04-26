#!/bin/bash
echo "Uninstallation FreeShip from the system"
# should be started as root or with sudo

touch /usr/bin/FreeShip
if [ $? -ne 0 ]; then
  echo "ERROR: You do not have privileges to uninstall FreeShip on system level."
  echo "Try to execute with sudo or as root."
  exit 1
fi

FS_HOME=/usr/share/FreeShip
FS_APP=${FS_HOME}


CFG=/etc/FreeShip/FreeShip.ini
[ -f $CFG ] && mv $CFG $CFG.0

echo "  Uninstall executables"
[ -x /usr/bin/FreeShip ] && rm /usr/bin/FreeShip


cd $FS_HOME/install/

if [ -x "`which xdg-desktop-menu 2>/dev/null`" ]; then
    echo "  Uninstall Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu uninstall --novendor --mode system Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu possibly not uninstalled. xdg-desktop-menu not found."
fi

if [ -x "`which xdg-mime 2>/dev/null`" ]; then
    echo "  Uninstall MIME"
    xdg-mime uninstall --mode system /usr/share/mime/packages/application.freeship-model-fbm.xml
    xdg-mime uninstall --mode system /usr/share/mime/packages/application.freeship-model-ftm.xml
else
 echo "Warning: MIME possibly not uninstalled. xdg-mime not found."
fi

if [ -x "`which xdg-icon-resource 2>/dev/null`" ]; then
    echo "  Uninstall Icons"
    for SZ in 16 24 32 48 64 96 128; do
      xdg-icon-resource uninstall --novendor --context apps --mode system --size ${SZ} freeship
      xdg-icon-resource uninstall --novendor --context mimetypes --mode system --size ${SZ} application-freeship-model-fbm
      xdg-icon-resource uninstall --novendor --context mimetypes --mode system --size ${SZ} application-freeship-model-ftm
    done
    xdg-icon-resource forceupdate
else
 echo "Warning: Icons possibly not uninstalled. xdg-icon-resource not found."
fi

if [ -x "`which update-desktop-database 2>/dev/null`" ]; then
    echo "  Update Desktop database"
    update-desktop-database /usr/share/applications
else
 echo "Warning: Desktop database not updated. update-desktop-database not found."
fi

if [ -x "`which update-mime-database 2>/dev/null`" ]; then
    echo "  Update MIME database"
    update-mime-database /usr/share/mime
else
 echo "Warning: MIME database not updated. update-mime-database not found."
fi

cd -

#change ownership of .local files of current user back to the user,
#because above commands may change them to root:root
GRP=$(ls -ld ~ | while read P X USR GRP S ; do echo $GRP; done)
USR=$(ls -ld ~ | while read P X USR GRP S ; do echo $USR; done)
[ -d ~/.local/share/applications ] && chown -R $USR:$GRP ~/.local/share/applications
[ -d ~/.local/share/icons ] || chown -R $USR:$GRP ~/.local/share/icons
[ -d ~/.local/share/mime ] || chown -R $USR:$GRP ~/.local/share/mime

echo "  Delete files"
[ -d /usr/share/FreeShip ] && rm -rf /usr/share/FreeShip

echo "Done"

