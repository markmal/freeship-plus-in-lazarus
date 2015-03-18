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

[ -d ${FS_APP} ] && rm -rf ${FS_APP}

CFG=/etc/FreeShip.ini
[ -f $CFG ] && mv $CFG $CFG.0

echo "  Uninstall executables"
[ -x /usr/bin/FreeShip ] && rm /usr/bin/FreeShip

# Make menu item and MIME types

echo "  Uninstall MIME"

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

xdg-mime uninstall --mode system application.freeship-model-fbm.xml
xdg-mime uninstall --mode system application.freeship-model-ftm.xml

rm application.freeship-model-fbm.xml application.freeship-model-ftm.xml

for SZ in 16 24 32 48; do
  xdg-icon-resource uninstall --context mimetypes --mode system --size ${SZ} freeship-${SZ}.png application/freeship-model-fbm
  xdg-icon-resource uninstall --context mimetypes --mode system --size ${SZ} freeship-${SZ}.png application/freeship-model-ftm
done

echo "  Update Desktop database"
update-desktop-database /usr/share/applications
echo "  Update MIME database"

update-mime-database /usr/share/mime

remove_mime(){
 if [ -f $1 ] ; then
   echo "      remove MIME defaults from $1"
   sed -i '/application\/freeship-model-fbm=freeship\.desktop/d' $1
   sed -i '/application\/freeship-model-ftm=freeship\.desktop/d' $1
 fi
}

#cleanup users MIME defaults. Just in case xdg-mime uninstall has not done it
#for mimefile in mimeapps defaults; do
#remove_mime /etc/xdg/$desktop-${mimefile}.list
#remove_mime /etc/xdg/${mimefile}.list
#remove_mime /usr/local/share/applications/$desktop-${mimefile}.list
#remove_mime /usr/share/applications/$desktop-${mimefile}.list
#remove_mime /usr/local/share/applications/${mimefile}.list
#remove_mime /usr/share/applications/${mimefile}.list
#done

for F in $(find /etc -name "*defaults.list") $(find /etc -name "*mimeapps.list") \
  $(find /usr -name "*defaults.list") $(find /usr -name "*mimeapps.list")
do 
  if grep 'application\/freeship-model-f[bt]m=freeship\.desktop' $F >/dev/null 2>&1; then 
    remove_mime $F
  fi;
done

sed -i "/^\s*text\/x-freeship-model-ftm\s\s*fbm\s\s*ftm\s*$/d" /etc/mime.types

echo "  Uninstall Menu"

for SZ in 16 24 32 48; do
  xdg-icon-resource uninstall --novendor --mode system --size ${SZ} freeship-${SZ}.png freeship
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

xdg-desktop-menu uninstall --novendor --mode system Engineering.directory freeship.desktop
xdg-desktop-menu forceupdate

rm freeship.desktop

echo "  Update Desktop database"
update-desktop-database /usr/share/applications

echo "Done"

