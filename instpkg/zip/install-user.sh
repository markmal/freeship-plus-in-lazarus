#!/bin/bash
echo "Installation FreeShip into user home"

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

echo "  Install executables"
# copy executable
[ -d "$FS_BIN" ] || mkdir "$FS_BIN"
cp FreeShip "${FS_BIN}"
chmod 755 "${FS_BIN}/FreeShip"

[ -d "${FS_HOME}/Import" ] || mkdir -p "${FS_HOME}/Import"

[ -d "${FS_PROJ}/Ships" ]  || mkdir -p "${FS_PROJ}/Ships"
[ -d "${FS_PROJ}/Import" ]  || mkdir -p "${FS_PROJ}/Import"
[ -d "${FS_PROJ}/Export" ]  || mkdir -p "${FS_PROJ}/Export"

[ -d "${FS_CACHE}" ] || mkdir -p "${FS_CACHE}"

FS_APP=${FS_HOME}

# these files are needed for uninstall
cp uninstall-user.sh ${FS_APP}/
cp -r install ${FS_APP}/

[ -d "${FS_HOME}/Exec" ]   || mkdir -p "${FS_APP}/Exec"

cp -r Languages "${FS_APP}"/
cp -r Manuals "${FS_APP}"/
cp -r Ships "${FS_APP}"/
cp -r Themes "${FS_APP}"/
cp -r install "${FS_APP}"/
cp -r locale "${FS_APP}"/

for F in uninstall-user.sh install-HOWTO.txt copyright COPYING README.txt
do
 cp $F "${FS_APP}"/
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

#Global default config
GCFG="${FS_HOME}"/FreeShip.ini
echo "[Directories]
LanguagesDirectory=${FS_APP}/Languages
ExecDirectory=${FS_HOME}/Exec
ManualsDirectory=${FS_HOME}/Manuals
TempDirectory=${FS_CACHE}
GlobalOpenDirectory=${FS_HOME}/Ships
GlobalImportDirectory=${FS_HOME}/Import
OpenDirectory=${FS_PROJ}/Ships
SaveDirectory=${FS_PROJ}/Ships
ImportDirectory=${FS_PROJ}/Ships
ExportDirectory=${FS_PROJ}/Ships
MenuIconDirectory=${FS_HOME}/Themes/Default/icons/16

[Graphic]
MenuIconSize=16
ToolIconSize=24
Theme=Default

[General]
Language=English
FbmEncoding=cp1252
MaxUndoMemory=20" >"$GCFG"

#User config
[ -d "${FS_CONF}" ] || mkdir "${FS_CONF}"
CFG="${FS_CONF}"/FreeShip.ini

if [ -f "$CFG" ]
then
  read -e -p "Configuration file ${CFG} already exits. 
Would you like to change current directories to default ones? 
Your current configuration will be backed up if you answer Y.
[Y/N]: " -i "Y" CHANGE

  if [ $CHANGE = "Y" ]; then
    cp "${CFG}" "${CFG}.bkp"
    echo "Your current configuration is backed up to ${CFG}.bkp"
    FSAPPD=${FS_APP//\//\\\/}
    sed -e 's/\(LanguagesDirectory=\)\(.*\)$/\1'"${FSAPPD}\/Languages/" \
        -e 's/\(ExecDirectory=\)\(.*\)$/\1'"${FSAPPD}\/Exec/" \
        -e 's/\(ManualsDirectory=\)\(.*\)$/\1'"${FSAPPD}\/Manuals/" ${CFG}.bkp >$CFG
  fi

else
  cp "$GCFG" "$CFG"
fi

# Make menu item and MIME types

echo \
"[Desktop Entry]
Version=1.0
Type=Application
Name=FreeShip Plus (user)
Comment=FREE!ship Plus for Linux
Icon=freeship
Exec="${FS_BIN}/FreeShip" %f
Path="${FS_HOME}/FreeShip"
NoDisplay=false
Categories=Engineering;
Keywords=Engineering;FreeShip;Designer;Vessel;
MimeType=application/freeship-model-ftm;application/freeship-model-fbm;
StartupWMClass=FreeShip
StartupNotify=false
Terminal=false
" > $FS_HOME/install/freeship.desktop

cd $FS_HOME/install/

if [ -x "$(which xdg-desktop-menu 2>/dev/null)" ]; then
    echo "  Install Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu install --novendor --mode user Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu not installed. xdg-desktop-menu not found."
fi

if [ -x "$(which xdg-mime 2>/dev/null)" ]; then
    echo "  Install MIME"
    xdg-mime install --mode user application.freeship-model-fbm.xml
    xdg-mime install --mode user application.freeship-model-ftm.xml
    xdg-mime default freeship.desktop application/freeship-model-fbm
    xdg-mime default freeship.desktop application/freeship-model-ftm
else
 echo "Warning: MIME not installed. xdg-mime not found."
fi

if [ -x "$(which xdg-icon-resource 2>/dev/null)" ]; then
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

echo "Done"
echo "FreeShip is installed into $FS_APP"
echo "  to uninstall enter into $FS_APP and execute uninstall-user.sh"

#if ldd FreeShip|grep "libQt4Pas\.so\.5.*not found" >/dev/null
#then echo "Warning! Free Pascal Qt4 Binding is not installed. Install package libqt4pas5"
#fi

