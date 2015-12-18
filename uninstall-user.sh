echo "Uninstallation FreeShip from user home"

FS_HOME=${HOME}/FreeShip

FS_APP=${FS_HOME}

echo "  Uninstall files"
[ -d ${FS_HOME}/Exec ]      && rm -rf ${FS_APP}/Exec
[ -d ${FS_HOME}/Languages ] && rm -rf ${FS_APP}/Languages
[ -d ${FS_HOME}/Manuals ]   && rm -rf ${FS_APP}/Manuals
[ -d ${FS_HOME}/Themes ]   && rm -rf ${FS_APP}/Themes
[ -d ${FS_HOME}/Temp ]   && rm -rf ${FS_APP}/Temp

[ -x ${HOME}/bin/FreeShip ] && rm -f ${HOME}/bin/FreeShip


echo "  Uninstall configuration"
CFG=${HOME}/.config/FreeShip/FreeShip.ini
[ -f $CFG ] && mv $CFG $CFG.0
echo "    your configuration saved as $CFG.0"

cd $FS_HOME/install/

if [ -x "`which xdg-desktop-menu 2>/dev/null`" ]; then
    echo "  Uninstall Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu uninstall --novendor --mode user Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu possibly not uninstalled. xdg-desktop-menu not found."
fi

if [ -x "`which xdg-mime 2>/dev/null`" ]; then
    echo "  Uninstall MIME"
    xdg-mime uninstall --mode user application.freeship-model-fbm.xml
    xdg-mime uninstall --mode user application.freeship-model-ftm.xml
else
 echo "Warning: MIME possibly not uninstalled. xdg-mime not found."
fi

if [ -x "`which xdg-icon-resource 2>/dev/null`" ]; then
    echo "  Uninstall Icons"
    for SZ in 16 24 32 48 64 96 128; do
      xdg-icon-resource uninstall --novendor --context apps --mode user --size ${SZ} freeship
      xdg-icon-resource uninstall --novendor --context mimetypes --mode user --size ${SZ} application-freeship-model-fbm
      xdg-icon-resource uninstall --novendor --context mimetypes --mode user --size ${SZ} application-freeship-model-ftm
    done
    xdg-icon-resource forceupdate
else
 echo "Warning: Icons possibly not uninstalled. xdg-icon-resource not found."
fi

if [ -x "`which update-desktop-database 2>/dev/null`" ]; then
    echo "  Update Desktop database"
    update-desktop-database ~/.local/share/applications
else
 echo "Warning: Desktop database not updated. update-desktop-database not found."
fi

if [ -x "`which update-mime-database 2>/dev/null`" ]; then
    echo "  Update MIME database"
    update-mime-database ~/.local/share/mime
else
 echo "Warning: MIME database not updated. update-mime-database not found."
fi

cd -

rm -rf $FS_HOME

echo "Done"
