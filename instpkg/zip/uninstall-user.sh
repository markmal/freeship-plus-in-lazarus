#!/bin/bash
echo "Uninstallation FreeShip from user home"

if [ -x "$(which systemd-path 2>/dev/null)" ]; then
  USER_BIN=$(systemd-path user-binaries)
  USER_LIB=$(systemd-path user-library-private)
  USER_CONF=$(systemd-path user-configuration)
  USER_CACHE=$(systemd-path user-state-cache)
  USER_SHARE=$(systemd-path user-shared)
  USER_DOCS=$(systemd-path user-documents)
else 
  echo "Warning: systemd-path is not found will install into $HOME/.local"
fi

FS_HOME=${USER_SHARE:-${HOME}/.local/share}/FreeShip
FS_BIN=${USER_BIN:-${HOME}/.local/bin}
FS_LIB=${USER_BIN:-${HOME}/.local/lib}
FS_CONF=${USER_CONF:-${HOME}/.config/FreeShip}
FS_CACHE=${USER_CONF:-${HOME}/.cache/FreeShip}
FS_PROJ=${USER_DOCS:-${HOME}/Documents/FreeShip}

FS_APP=${FS_HOME}

echo "  Uninstall configuration"
CFG=~/.config/FreeShip/FreeShip.ini
[ -f $CFG ] && mv $CFG $CFG.bkp
echo "    your configuration saved as $CFG.bkp"

# delete FreeShip user project directories if empty
cd "${FS_PROJ}"
[ ! "$(ls -A Ships)" ]  && rmdir Ships
[ ! "$(ls -A Import)" ] && rmdir Import
[ ! "$(ls -A Export)" ] && rmdir Export
cd -
[ ! "$(ls -A '$FS_PROJ')" ] && rmdir "$FS_PROJ"

cd $FS_HOME/install/

if [ -x "$(which xdg-desktop-menu 2>/dev/null)" ]; then
    echo "  Uninstall Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu uninstall --novendor --mode user Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu possibly not uninstalled. xdg-desktop-menu not found."
fi

if [ -x "$(which xdg-mime 2>/dev/null)" ]; then
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

if [ -x "$(which update-desktop-database 2>/dev/null)" ]; then
    echo "  Update Desktop database"
    update-desktop-database "${USER_SHARE}/applications"
else
 echo "Warning: Desktop database not updated. update-desktop-database not found."
fi

if [ -x "$(which update-mime-database 2>/dev/null)" ]; then
    echo "  Update MIME database"
    update-mime-database "${USER_SHARE}/mime"
else
 echo "Warning: MIME database not updated. update-mime-database not found."
fi

cd -

echo "  Uninstall files from ${FS_HOME}"
[ -z "${FS_HOME}" ] && exit 1
[ -d "${FS_HOME}/Exec" ]      && rm -rf "${FS_HOME}/Exec"
[ -d "${FS_HOME}/Languages" ] && rm -rf "${FS_HOME}/Languages"
[ -d "${FS_HOME}/Manuals" ]   && rm -rf "${FS_HOME}/Manuals"
[ -d "${FS_HOME}/Themes" ]    && rm -rf "${FS_HOME}/Themes"
[ -d "${FS_HOME}/Temp" ]      && rm -rf "${FS_HOME}/Temp"
[ -d "${FS_HOME}/locale" ]    && rm -rf "${FS_HOME}/locale"
[ -d "${FS_HOME}/install" ]   && rm -rf "${FS_HOME}/install"
[ -d "${FS_HOME}/Ships" ]     && rm -rf "${FS_HOME}/Ships"
[ -d "${FS_HOME}/Import" ]    && rm -rf "${FS_HOME}/Import"
cd "${FS_HOME}"
rm -f COPYING copyright FreeShip.ini install-HOWTO.txt README.txt uninstall-user.sh uninstall-system.sh
cd -

[ -x "${FS_BIN}/FreeShip" ] && rm -f "${FS_BIN}/FreeShip"

rmdir "$FS_HOME"

echo "Done"
