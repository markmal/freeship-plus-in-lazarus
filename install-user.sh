echo "Installation FreeShip into user home"

FS_HOME=${HOME}/FreeShip

echo "  Install executables"
# copy executable
[ -d ${HOME}/bin ] || mkdir ${HOME}/bin
cp FreeShip ${HOME}/bin/

[ -d ${FS_HOME}/Export ] || mkdir -p ${FS_HOME}/Export
[ -d ${FS_HOME}/Import ] || mkdir -p ${FS_HOME}/Import
[ -d ${FS_HOME}/Ships ]  || mkdir -p ${FS_HOME}/Ships
[ -d ${FS_HOME}/Temp ]   || mkdir -p ${FS_HOME}/Temp

FS_APP=${FS_HOME}

# these files are needed for uninstall
cp uninstall-user.sh ${FS_APP}/
#cp application.freeship-model-fbm.xml ${FS_APP}/
#cp application.freeship-model-ftm.xml ${FS_APP}/

[ -d ${FS_HOME}/Exec ]   || mkdir -p ${FS_APP}/Exec

cp -r Languages ${FS_APP}/
cp -r Manuals ${FS_APP}/
cp -r Ships ${FS_APP}/

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

echo "  Install Menu"
for SZ in 16 24 32 48; do
  xdg-icon-resource install --novendor --mode user --size ${SZ} freeship-${SZ}.png freeship
done

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
" > freeship.desktop

# Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
# We add our own Engineering top level menu folder and place FreeShip launcher into it.
xdg-desktop-menu install --novendor --mode user Engineering.directory freeship.desktop
xdg-desktop-menu forceupdate

## manually move Engineering menu item from bottom to above Games (for simplicity). I do not know a standard way to do it.
CURDIR=$(pwd)
cd ~/.config/menus
MENUFILE=$(grep -lr '^\s*<Menuname>Games</Menuname>$' * )
if [ ! -z $MENUFILE ]; then
  sed -i '/<Menuname>Engineering<\/Menuname>/d' ${MENUFILE}
  sed -i 's/^\s*<Menuname>Games<\/Menuname>/\t\t<Menuname>Engineering<\/Menuname>\n&/' ${MENUFILE}
fi
cd $CURDIR
##

echo "  Install MIME"

echo '<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
    <mime-type type="application/freeship-model-ftm">
        <comment>FREE!Ship Plus model (text)</comment>
        <icon name="freeship"/>
        <glob pattern="*.ftm"/>
    </mime-type>
</mime-info>' >application.freeship-model-ftm.xml

echo '<?xml version="1.0" encoding="UTF-8"?>
<mime-info xmlns="http://www.freedesktop.org/standards/shared-mime-info">
    <mime-type type="application/freeship-model-fbm">
        <comment>FREE!Ship Plus model (binary)</comment>
        <icon name="freeship"/>
        <glob pattern="*.fbm"/>
    </mime-type>
</mime-info>' > application.freeship-model-fbm.xml

xdg-mime install --mode user application.freeship-model-fbm.xml
xdg-mime install --mode user application.freeship-model-ftm.xml

rm application.freeship-model-fbm.xml application.freeship-model-ftm.xml

for SZ in 16 24 32 48; do
  xdg-icon-resource install --context mimetypes --mode user --size ${SZ} freeship-${SZ}.png application/freeship-model-fbm
  xdg-icon-resource install --context mimetypes --mode user --size ${SZ} freeship-${SZ}.png application/freeship-model-ftm
done

xdg-mime default freeship.desktop application/freeship-model-fbm
xdg-mime default freeship.desktop application/freeship-model-ftm

echo "  Update Desktop database"
update-desktop-database ~/.local/share/applications
echo "  Update MIME database"
update-mime-database ~/.local/share/mime

echo "Done"
echo "FreeShip is installed into $FS_APP"
echo "  to uninstall enter into $FS_APP and execute uninstall-user.sh"

if ldd FreeShip|grep "libQt4Pas\.so\.5.*not found" >/dev/null
then echo "Warning! Free Pascal Qt4 Binding is not installed. Install package libqt4pas5"
fi

