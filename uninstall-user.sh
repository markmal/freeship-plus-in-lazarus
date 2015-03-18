echo "Uninstallation FreeShip into user home"

FS_HOME=${HOME}/FreeShip

FS_APP=${FS_HOME}

echo "  Uninstall files"

[ -d ${FS_HOME}/Exec ]      && rm -rf ${FS_APP}/Exec
[ -d ${FS_HOME}/Languages ] && rm -rf ${FS_APP}/Languages
[ -d ${FS_HOME}/Manuals ]   && rm -rf ${FS_APP}/Manuals

[ -x ${HOME}/bin/FreeShip ] && rm -f ${HOME}/bin/FreeShip

echo "  Uninstall configuration"
CFG=${HOME}/.config/FreeShip/FreeShip.ini
[ -f $CFG ] && mv $CFG $CFG.0
echo "    your configuration saved as $CFG.0"

# Remove menu item and MIME types

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

xdg-mime uninstall --mode user application.freeship-model-fbm.xml
xdg-mime uninstall --mode user application.freeship-model-ftm.xml

rm application.freeship-model-fbm.xml application.freeship-model-ftm.xml


for SZ in 16 24 32 48; do
  xdg-icon-resource uninstall --context mimetypes --mode user --size ${SZ} freeship-${SZ}.png application/freeship-model-fbm
  xdg-icon-resource uninstall --context mimetypes --mode user --size ${SZ} freeship-${SZ}.png application/freeship-model-ftm
done

remove_mime(){
 if [ -f $1 ] ; then
   echo "    remove MIME default from $1"
   sed -i '/application\/freeship-model-fbm=freeship\.desktop/d' $1
   sed -i '/application\/freeship-model-ftm=freeship\.desktop/d' $1
 fi 
}

#cleanup users MIME defaults. Just in case xdg-mime uninstall has not done it
#remove_mime $HOME/.config/$desktop-mimeapps.list
#remove_mime $HOME/.config/mimeapps.list
#remove_mime $HOME/.local/share/applications/$desktop-mimeapps.list
#remove_mime $HOME/.local/share/applications/mimeapps.list
#remove_mime $HOME/.config/$desktop-defaults.list
#remove_mime $HOME/.config/defaults.list
#remove_mime $HOME/.local/share/applications/$desktop-defaults.list
#remove_mime $HOME/.local/share/applications/defaults.list

for F in $(find $HOME/.config -name "*defaults.list") $(find $HOME/.local -name "*mimeapps.list")
do
  if grep 'application\/freeship-model-f[bt]m=freeship\.desktop' $F >/dev/null 2>&1; then
    remove_mime $F
  fi;
done

echo "  Update MIME database"
update-mime-database ~/.local/share/mime
# ----------------

echo "  Uninstall Menu"
for SZ in 16 24 32 48; do
  xdg-icon-resource uninstall --mode user --size ${SZ} freeship-${SZ}.png freeship
done

echo \
"[Desktop Entry]
Version=1.0
Type=Application
Name=FreeShip Plus
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

echo if this hangs, press ^C
xdg-desktop-menu uninstall  --mode user Engineering.directory freeship.desktop 
xdg-desktop-menu forceupdate

rm freeship.desktop

echo "  Update Desktop database"
update-desktop-database ~/.local/share/applications
echo "  Update MIME database"
update-mime-database    ~/.local/share/mime

echo "Done"