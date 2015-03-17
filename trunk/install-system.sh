echo Installation FreeShip into the system
# should be started as root or with sudo

echo "  Install executables"
cp FreeShip /usr/bin/
if [ $? -ne 0 ]; then
  echo "ERROR: You do not have privileges to install FreeShip on system level."
  echo "Try to execute with sudo or as root."
  exit 1
fi

FS_HOME=/usr/share/FreeShip

[ -d ${FS_HOME}/Import ] || mkdir -p ${FS_HOME}/Import
[ -d ${FS_HOME}/Ships ]  || mkdir -p ${FS_HOME}/Ships

FS_APP=${FS_HOME}
cp uninstall-system.sh ${FS_APP}

[ -d ${FS_HOME}/Exec ]   || mkdir -p ${FS_APP}/Exec

cp -r Languages ${FS_APP}/
cp -r Manuals ${FS_APP}/
cp -r Ships ${FS_APP}/

echo "  Install configuration"
CFG=/etc/FreeShip.ini
install -b FreeShip.ini $CFG

echo "  Install Menu"
for SZ in 16 24 32 48; do
  xdg-icon-resource install --novendor --mode system --size ${SZ} freeship-${SZ}.png freeship
done

echo \
'[Desktop Entry]
Version=1.0
Type=Application
Name=FreeShip Plus
Comment=FREE!ship Plus for Linux
Icon=freeship
Exec=/usr/bin/FreeShip %f
Path=/usr/share/FreeShip
NoDisplay=false
Categories=Engineering;
Keywords=Engineering;FreeShip;Designer;Vessel;
MimeType=application/freeship-model-ftm;application/freeship-model-fbm;
StartupWMClass=FreeShip
StartupNotify=false
Terminal=false
' > freeship.desktop

# Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
# We add our own Engineering top level menu folder and place FreeShip launcher into it.
xdg-desktop-menu install --novendor --mode system Engineering.directory freeship.desktop
xdg-desktop-menu forceupdate

## manually move Engineering menu item from bottom to above Games (for simplicity). I do not know a standard way to do it.
CURDIR=$(pwd)
cd /etc/xdg
MENUFILE=$(grep -lr '^\s*<Menuname>Games</Menuname>$' * )
if [ ! -z $MENUFILE ]; then
  sed -i '/<Menuname>Engineering<\/Menuname>/d' ${MENUFILE}
  sed -i 's/^\s*<Menuname>Games<\/Menuname>/\t\t<Menuname>Engineering<\/Menuname>\n&/' ${MENUFILE}
fi
cd $CURDIR
##

echo "  Install MIME"
grep "^\s*text/x-freeship-model-ftm\s\s*fbm\s\s*ftm\s*$" /etc/mime.types
[ $? -eq 0 ] || echo "text/x-freeship-model-ftm			fbm ftm" >> /etc/mime.types

xdg-mime install --mode system application.freeship-model-fbm.xml
xdg-mime install --mode system application.freeship-model-ftm.xml

for SZ in 16 24 32 48; do
  xdg-icon-resource install --context mimetypes --mode system --size ${SZ} freeship-${SZ}.png application/freeship-model-fbm
  xdg-icon-resource install --context mimetypes --mode system --size ${SZ} freeship-${SZ}.png application/freeship-model-ftm
done

xdg-mime default freeship.desktop application/freeship-model-fbm
xdg-mime default freeship.desktop application/freeship-model-ftm

echo "  Update Desktop database"
update-desktop-database /usr/share/applications
echo "  Update MIME database"
update-mime-database /usr/share/mime

echo "Done"


