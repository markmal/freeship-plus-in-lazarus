echo Installation FreeShip into the system
# should be started as root or with sudo

echo "  Install executables"
cp FreeShip /usr/bin/
if [ $? -ne 0 ]; then
  echo "ERROR: You do not have privileges to install FreeShip on system level."
  echo "Try to execute with sudo or as root."
  exit 1
fi
chmod 755 /usr/bin/FreeShip

FS_HOME=/usr/share/FreeShip

[ -d ${FS_HOME}/Import ] || mkdir -p ${FS_HOME}/Import
[ -d ${FS_HOME}/Languages ] || mkdir -p ${FS_HOME}/Languages
[ -d ${FS_HOME}/Manuals ] || mkdir -p ${FS_HOME}/Manuals
[ -d ${FS_HOME}/Ships ]  || mkdir -p ${FS_HOME}/Ships
[ -d ${FS_HOME}/Themes ]  || mkdir -p ${FS_HOME}/Themes
[ -d ${FS_HOME}/Temp ]  || mkdir -p ${FS_HOME}/Temp

FS_APP=${FS_HOME}

[ -d ${FS_HOME}/Exec ]   || mkdir -p ${FS_APP}/Exec

cp -r Languages ${FS_APP}/
cp -r Manuals ${FS_APP}/
cp -r Ships ${FS_APP}/
cp -r Themes ${FS_APP}/
cp -r install ${FS_APP}/
cp "GNU General Public License (GPL).txt" ${FS_APP}/

for F in Whatsnew.txt uninstall-system.sh install-HOWTO.txt copyright
do
 cp $F ${FS_APP}/
done

echo "  Install configuration"
CFG=/etc/FreeShip/FreeShip.ini
[ -d /etc/FreeShip ] || mkdir /etc/FreeShip
cp install/FreeShip.ini $CFG

cd $FS_HOME/install/

if [ -x "`which xdg-desktop-menu 2>/dev/null`" ]; then
    echo "  Install Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu install --novendor --mode system Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu not installed. xdg-desktop-menu not found."
fi

if [ -x "`which xdg-mime 2>/dev/null`" ]; then
    echo "  Install MIME"
    xdg-mime install --mode system application.freeship-model-fbm.xml
    xdg-mime install --mode system application.freeship-model-ftm.xml
    xdg-mime default freeship.desktop application/freeship-model-fbm
    xdg-mime default freeship.desktop application/freeship-model-ftm
else
 echo "Warning: MIME not installed. xdg-mime not found."
fi

if [ -x "`which xdg-icon-resource 2>/dev/null`" ]; then
    echo "  Install Icons"
    for SZ in 16 24 32 48 64 96 128; do
      ICON=$FS_HOME/Themes/Default/icons/${SZ}/00-freeship.png
      xdg-icon-resource install --novendor --context apps --mode system --size ${SZ} $ICON freeship
      xdg-icon-resource install --novendor --context mimetypes --mode system --size ${SZ} $ICON application-freeship-model-fbm
      xdg-icon-resource install --novendor --context mimetypes --mode system --size ${SZ} $ICON application-freeship-model-ftm
    done
    xdg-icon-resource forceupdate
else
 echo "Warning: Icons not installed. xdg-icon-resource not found."
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
#because above commands may chane them to root:root
GRP=$(ls -ld ~ | while read P X USR GRP S ; do echo $GRP; done)
USR=$(ls -ld ~ | while read P X USR GRP S ; do echo $USR; done)
[ -d ~/.local/share/applications ] && chown -R $USR:$GRP ~/.local/share/applications
[ -d ~/.local/share/icons ] || chown -R $USR:$GRP ~/.local/share/icons
[ -d ~/.local/share/mime ] || chown -R $USR:$GRP ~/.local/share/mime

echo "Done"
echo "FreeShip is installed into $FS_APP"
echo "  to uninstall enter into $FS_APP and execute uninstall-system.sh"

if ldd FreeShip|grep "libQt4Pas\.so\.5.*not found" >/dev/null
then echo "Warning! Free Pascal Qt4 Binding is not installed. Install package libqt4pas5"
fi
