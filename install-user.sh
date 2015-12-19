#!/bin/bash
echo "Installation FreeShip into user home"

FS_HOME=${HOME}/FreeShip

echo "  Install executables"
# copy executable
[ -d ${HOME}/bin ] || mkdir ${HOME}/bin
cp FreeShip ${HOME}/bin/
chmod 755 ${HOME}/bin/FreeShip

[ -d ${FS_HOME}/Export ] || mkdir -p ${FS_HOME}/Export
[ -d ${FS_HOME}/Import ] || mkdir -p ${FS_HOME}/Import
[ -d ${FS_HOME}/Ships ]  || mkdir -p ${FS_HOME}/Ships
[ -d ${FS_HOME}/Temp ]   || mkdir -p ${FS_HOME}/Temp
[ -d ${FS_HOME}/Themes ]  || mkdir -p ${FS_HOME}/Themes

FS_APP=${FS_HOME}

# these files are needed for uninstall
cp uninstall-user.sh ${FS_APP}/
cp -r install ${FS_APP}/

[ -d ${FS_HOME}/Exec ]   || mkdir -p ${FS_APP}/Exec

cp -r Languages ${FS_APP}/
cp -r Manuals ${FS_APP}/
cp -r Ships ${FS_APP}/
cp -r Themes ${FS_APP}/
cp -r install ${FS_APP}/

cp "GNU General Public License (GPL).txt" ${FS_APP}/

for F in Whatsnew.txt uninstall-user.sh install-HOWTO.txt copyright
do
 cp $F ${FS_APP}/
done

# save original ships lists that will be used when uninstall-user.sh executed to remove only files that were originally installed,
# preserving files created or modified by user
# original-ships.md5 is list of files with their md5 checksums. It is created with this command

cd Ships/

find . -type f -exec md5sum \{\} \; > .original-ships.md5

# save original dirs
find . -type d | grep -v '^.$' > .original-ship-dirs.lst

cd ..


echo "  Install configuration"
[ -d ${HOME}/.config/FreeShip ] || mkdir ${HOME}/.config/FreeShip
CFG=${HOME}/.config/FreeShip/FreeShip.ini

if [ -f $CFG ]
then
  read -e -p "Configuration file ${CFG} already exits. 
Whould you like to change shared directories to user ones? [Y/N]: " -i "Y" CHANGE
  
  if [ $CHANGE = "Y" ]; then
    cp ${CFG} ${CFG}.0
    echo "Old configuration is backed up to ${CFG}.0"
    FSAPPD=${FS_APP//\//\\\/}
    sed -e 's/\(LanguagesDirectory=\)\(.*\)$/\1'"${FSAPPD}\/Languages/" \
        -e 's/\(ExecDirectory=\)\(.*\)$/\1'"${FSAPPD}\/Exec/" \
        -e 's/\(ManualsDirectory=\)\(.*\)$/\1'"${FSAPPD}\/Manuals/" ${CFG}.0 >$CFG
  fi

else
  echo "[Directories]" >$CFG
  echo "LanguagesDirectory=${FS_APP}/Languages" >>$CFG
  echo "ExecDirectory=${FS_APP}/Exec" >>$CFG
  echo "ManualsDirectory=${FS_APP}/Manuals" >>$CFG
  echo "TempDirectory=${FS_HOME}/Temp" >>$CFG
  echo "OpenDirectory=${FS_APP}/Ships" >>$CFG
  echo "SaveDirectory=${FS_HOME}/Ships" >>$CFG
  echo "ImportDirectory=${FS_APP}/Import" >>$CFG
  echo "ExportDirectory=${FS_HOME}/Export" >>$CFG
fi


# Make menu item and MIME types

echo \
"[Desktop Entry]
Version=1.0
Type=Application
Name=FreeShip Plus (user)
Comment=FREE!ship Plus for Linux
Icon=freeship
Exec=${HOME}/bin/FreeShip %f
Path=${HOME}/FreeShip
NoDisplay=false
Categories=Engineering;
Keywords=Engineering;FreeShip;Designer;Vessel;
MimeType=application/freeship-model-ftm;application/freeship-model-fbm;
StartupWMClass=FreeShip
StartupNotify=false
Terminal=false
" > $FS_HOME/install/freeship.desktop



cd $FS_HOME/install/

if [ -x "`which xdg-desktop-menu 2>/dev/null`" ]; then
    echo "  Install Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu install --novendor --mode user Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu not installed. xdg-desktop-menu not found."
fi

if [ -x "`which xdg-mime 2>/dev/null`" ]; then
    echo "  Install MIME"
    xdg-mime install --mode user application.freeship-model-fbm.xml
    xdg-mime install --mode user application.freeship-model-ftm.xml
    xdg-mime default freeship.desktop application/freeship-model-fbm
    xdg-mime default freeship.desktop application/freeship-model-ftm
else
 echo "Warning: MIME not installed. xdg-mime not found."
fi

if [ -x "`which xdg-icon-resource 2>/dev/null`" ]; then
    echo "  Install Icons"
    for SZ in 16 24 32 48 64 96 128; do
      ICON=$FS_HOME/Themes/Default/icons/${SZ}/00-freeship.png
      xdg-icon-resource install --novendor --context apps --mode user --size ${SZ} $ICON freeship
      xdg-icon-resource install --novendor --context mimetypes --mode user --size ${SZ} $ICON application-freeship-model-fbm
      xdg-icon-resource install --novendor --context mimetypes --mode user --size ${SZ} $ICON application-freeship-model-ftm
    done
else
 echo "Warning: Icons not installed. xdg-icon-resource not found."
fi


if [ -x "`which update-desktop-database 2>/dev/null`" ]; then
    echo "  Update Desktop database"
    update-desktop-database $HOME/.local/share/applications
else
 echo "Warning: Desktop database not updated. update-desktop-database not found."
fi

if [ -x "`which update-mime-database 2>/dev/null`" ]; then
    echo "  Update MIME database"
    update-mime-database $HOME/.local/share/mime
else
 echo "Warning: MIME database not updated. update-mime-database not found."
fi

echo "Done"
echo "FreeShip is installed into $FS_APP"
echo "  to uninstall enter into $FS_APP and execute uninstall-user.sh"

if ldd FreeShip|grep "libQt4Pas\.so\.5.*not found" >/dev/null
then echo "Warning! Free Pascal Qt4 Binding is not installed. Install package libqt4pas5"
fi

