#!/bin/bash
echo "Installation of FreeShip"

function ShowGPLShortNotice() {
read -e -p "FreeShip is licensed under General Public License v3 and later (GPLv3+)
Copyright (C) 2005, 2006 Martijn van Engeland
    2007-2012 Victor F. Timoshenko
    2015 Mark Malakanov

    This program is free software: you can redistribute it and/or modify
    it under the terms of the GNU General Public License as published by
    the Free Software Foundation, either version 3 of the License, or
    (at your option) any later version.

    This program is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
    GNU General Public License for more details.

    You should have received a copy of the GNU General Public License
    along with this program.  If not, see <https://www.gnu.org/licenses/>.

W - show me short warranty part
G - show me full GPLv3 license
A - I ACCEPT THE LICENSE TERMS AND CONDITIONS
Q - Quit

[W|G|A|Q]: " RLICENSE
}

function ShowWarranty() {
echo "  15. Disclaimer of Warranty.

  THERE IS NO WARRANTY FOR THE PROGRAM, TO THE EXTENT PERMITTED BY
APPLICABLE LAW.  EXCEPT WHEN OTHERWISE STATED IN WRITING THE COPYRIGHT
HOLDERS AND/OR OTHER PARTIES PROVIDE THE PROGRAM "AS IS" WITHOUT WARRANTY
OF ANY KIND, EITHER EXPRESSED OR IMPLIED, INCLUDING, BUT NOT LIMITED TO,
THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
PURPOSE.  THE ENTIRE RISK AS TO THE QUALITY AND PERFORMANCE OF THE PROGRAM
IS WITH YOU.  SHOULD THE PROGRAM PROVE DEFECTIVE, YOU ASSUME THE COST OF
ALL NECESSARY SERVICING, REPAIR OR CORRECTION.

  16. Limitation of Liability.

  IN NO EVENT UNLESS REQUIRED BY APPLICABLE LAW OR AGREED TO IN WRITING
WILL ANY COPYRIGHT HOLDER, OR ANY OTHER PARTY WHO MODIFIES AND/OR CONVEYS
THE PROGRAM AS PERMITTED ABOVE, BE LIABLE TO YOU FOR DAMAGES, INCLUDING ANY
GENERAL, SPECIAL, INCIDENTAL OR CONSEQUENTIAL DAMAGES ARISING OUT OF THE
USE OR INABILITY TO USE THE PROGRAM (INCLUDING BUT NOT LIMITED TO LOSS OF
DATA OR DATA BEING RENDERED INACCURATE OR LOSSES SUSTAINED BY YOU OR THIRD
PARTIES OR A FAILURE OF THE PROGRAM TO OPERATE WITH ANY OTHER PROGRAMS),
EVEN IF SUCH HOLDER OR OTHER PARTY HAS BEEN ADVISED OF THE POSSIBILITY OF
SUCH DAMAGES." | less
}

# ------- BEGIN -------- #

# check key dependencies
X=0
for U in md5sum find less date diff; do 
  if [ ! -x "/usr/bin/$U" ] && [ ! -x "/bin/$U" ]; then
    echo "Need '$U'"; X=1
  fi  
done
[ $X -ne 0 ] && exit 1

# Get GPL acceptance
RLICENSE=N
while [ "$RLICENSE" != "A" ] 
do
 ShowGPLShortNotice
 case $RLICENSE in
  W|w) 
    ShowWarranty
    ;;
  G|g) 
    less COPYING
    ;;
  'A') 
    echo ACCEPTED
    ;;
  'a') 
    echo type capital A
    ;;
  Q|q) 
    echo Thanks. Bye.
    exit
    ;;
  *)
    ShowGPLShortNotice
 esac
done

[ "$RLICENSE" != "A" ] && exit

# Get installation scope
INST_SCOPE=N
while [ "$INST_SCOPE" == "N" ]
do
  read -e -p "Select installation scope
M - Per-machine. The program will be installed into system directories and will be available for all users.
U - Per-user. The program will be installed into user home directories and will be available for you only. 
    Please note this is less secure because executables can be vulnerable to malware.
Q - quit
[M|U]: " INST_SCOPE
 echo $INST_SCOPE
 case $INST_SCOPE in
  M|m) 
    INST_SCOPE=M
    ;;
  U|u) 
    INST_SCOPE=U
    ;;
  Q|q) 
    echo Thanks. Bye.
    exit
    ;;
  *)
    INST_SCOPE=N
    ;;
 esac
done

if [ "$INST_SCOPE" == "M" ]; then
  if [ "$(whoami)" != "root" ]; then
    echo "Please run this installation script as root or using 'sudo'"
    exit 1
  fi
  SHARE_DIR=/usr/share
  FS_HOME=/usr/share/FreeShip
  FS_BIN=/usr/local/bin
  FS_LIB=/usr/local/lib
  FS_GCONF=/etc/FreeShip
  XDG_MODE=system
fi

if [ "$INST_SCOPE" == "U" ]; then
 if [ -x "$(which systemd-path 2>/dev/null)" ]; then
  USER_BIN=$(systemd-path user-binaries)
  USER_LIB=$(systemd-path user-library-private)
  USER_CONF=$(systemd-path user-configuration)
  USER_CACHE=$(systemd-path user-state-cache)
  USER_SHARE=$(systemd-path user-shared)
  USER_DOCS=$(systemd-path user-documents)
  XDG_MODE=user
 else 
  echo "Warning: systemd-path is not found will install into $HOME/.local"
 fi

 SHARE_DIR=${USER_SHARE:-${HOME}/.local/share}
 FS_HOME=${USER_SHARE:-${HOME}/.local/share}/FreeShip
 FS_BIN=${USER_BIN:-${HOME}/.local/bin}
 FS_LIB=${USER_LIB:-${HOME}/.local/lib}
 FS_GCONF=$FS_HOME
 FS_CONF=${USER_CONF:-${HOME}/.config/FreeShip}
 FS_CACHE=${USER_CONF:-${HOME}/.cache/FreeShip}
 FS_PROJ=${USER_DOCS:-${HOME}/Documents/FreeShip}
fi

if [ -d "${FS_HOME}" ]; then
    [ -d "${FS_HOME}" ] && chmod -R u+rw "${FS_HOME}"
    echo "  Backup existing files by renaming:"
    D=$(date +'%Y-%m-%d_%T')
    (find . -type f) | while read FN; do
      if [ -f "${FS_HOME}/$FN" ]; then
        #S0=$(md5sum "${FS_HOME}/$FN")
        #S1=$(md5sum "$FN")
        #if [ "${S0:0:32}" != "${S1:0:32}" ]; then
        if ! diff -q "${FS_HOME}/$FN" "$FN" &>/dev/null ; then
          B="${FN}~$D"
          echo "    rename '${FS_HOME}/$FN' to '$B'"
          mv "${FS_HOME}/$FN" "${FS_HOME}/$B"
        fi  
      fi
    done
fi

echo "  Install executables"

cp FreeShip "${FS_BIN}/"

[ -d "${FS_HOME}" ] || mkdir -p "${FS_HOME}"
cp -r Exec "${FS_HOME}/"
cp -r Languages/ "${FS_HOME}/"
cp -r locale "${FS_HOME}/"
cp -r Manuals "${FS_HOME}/"
cp -r Ships "${FS_HOME}/"
cp -r Themes "${FS_HOME}/"
[ -d "${FS_HOME}/Import" ] || mkdir -p "${FS_HOME}/Import"
# these files are needed for uninstall
cp -r install ${FS_HOME}/
cp -r uninstall-FreeShip.sh ${FS_HOME}/

# save original ships lists that will be used when uninstall-FreeShip.sh executed 
# to remove only files that were originally installed,
# preserving files created or modified by user
# original-ships.md5 is list of files with their md5 checksums. It is created with this command
find . -type f -exec md5sum \{\} \; > ${FS_HOME}/install/.original-files.md5
# save original dirs
find . -type d | grep -v '^.$' > ${FS_HOME}/install/.original-dirs.lst

echo "INST_SCOPE=$INST_SCOPE
SHARE_DIR=$SHARE_DIR
FS_HOME=$FS_HOME
FS_BIN=$FS_BIN
FS_LIB=$FS_LIB
" > ${FS_HOME}/install/uninstall.var

if [ "$INST_SCOPE" == "U" ]; then
  [ -d "${FS_PROJ}/Ships" ]   || mkdir -p "${FS_PROJ}/Ships"
  [ -d "${FS_PROJ}/Import" ]  || mkdir -p "${FS_PROJ}/Import"
  [ -d "${FS_PROJ}/Export" ]  || mkdir -p "${FS_PROJ}/Export"
  [ -d "${FS_CACHE}" ] || mkdir -p "${FS_CACHE}"
fi

for F in uninstall-FreeShip.sh install-HOWTO.txt copyright COPYING README.txt
do
 cp $F "${FS_HOME}"/
done

echo "  Install configuration"

#Global default config
[ ! -d "${FS_GCONF}" ] && mkdir "${FS_GCONF}"
GCFG="${FS_GCONF}"/FreeShip.ini
echo "[Directories]
LanguagesDirectory=${FS_HOME}/Languages
ExecDirectory=${FS_HOME}/Exec
ManualsDirectory=${FS_HOME}/Manuals
GlobalOpenDirectory=${FS_HOME}/Ships
GlobalImportDirectory=${FS_HOME}/Import
MenuIconDirectory=${FS_HOME}/Themes/Default/icons/16

[Graphic]
MenuIconSize=16
ToolIconSize=24
Theme=Default

[General]
Language=English
FbmEncoding=cp1252
MaxUndoMemory=32
" > FreeShip.ini
if [ -f "$GCFG" ] && ! diff -q FreeShip.ini "$GCFG" &>/dev/null ; then
    echo "    rename existing '$GCFG'" \
    mv "$GCFG" "${GCFG}~"$(date +'%Y-%m-%d_%T')
fi    
cp FreeShip.ini "$GCFG"
chmod a-x "$GCFG"
md5sum "$GCFG" >> ${FS_HOME}/install/.original-files.md5

if [ "$INST_SCOPE" == "U" ]; then
  OpenDirectory=${FS_PROJ}/Ships
  SaveDirectory=${FS_PROJ}/Ships
  ImportDirectory=${FS_PROJ}/Ships
  ExportDirectory=${FS_PROJ}/Ships
  TempDirectory=${FS_CACHE}
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
" > "${FS_HOME}/install/freeship.desktop"

cd "${FS_HOME}/install/"

if [ -x "$(which xdg-desktop-menu 2>/dev/null)" ]; then
    echo "  Install Menu"
    # Due to standard Science/Engineering is "Additional" item, it is not presented in visible menu.
    # We add our own Engineering top level menu folder and place FreeShip launcher into it.
    xdg-desktop-menu install --novendor --mode "$XDG_MODE" Engineering.directory freeship.desktop
    xdg-desktop-menu forceupdate
else
 echo "Warning: Menu not installed. xdg-desktop-menu not found."
fi

if [ -x "$(which xdg-mime 2>/dev/null)" ]; then
    echo "  Install MIME"
    xdg-mime install --mode "$XDG_MODE" application.freeship-model-fbm.xml
    xdg-mime install --mode "$XDG_MODE" application.freeship-model-ftm.xml
    xdg-mime default freeship.desktop application/freeship-model-fbm
    xdg-mime default freeship.desktop application/freeship-model-ftm
else
 echo "Warning: MIME not installed. xdg-mime not found."
fi

if [ -x "$(which xdg-icon-resource 2>/dev/null)" ]; then
    echo "  Install Icons"
    for SZ in 16 24 32 48 64 96 128; do
      ICON="${FS_HOME}/Themes/Default/icons/${SZ}/00-freeship.png"
      xdg-icon-resource install --novendor --context apps --mode "$XDG_MODE" --size ${SZ} $ICON freeship
      xdg-icon-resource install --novendor --context mimetypes --mode "$XDG_MODE" --size ${SZ} "$ICON" application-freeship-model-fbm
      xdg-icon-resource install --novendor --context mimetypes --mode "$XDG_MODE" --size ${SZ} "$ICON" application-freeship-model-ftm
    done
else
 echo "Warning: Icons not installed. xdg-icon-resource not found."
fi

if [ -x "$(which update-desktop-database 2>/dev/null)" ]; then
    echo "  Update Desktop database"
    update-desktop-database "${SHARE_DIR}/applications"
else
 echo "Warning: Desktop database not updated. update-desktop-database not found."
fi

if [ -x "$(which update-mime-database 2>/dev/null)" ]; then
    echo "  Update MIME database"
    update-mime-database -n "${SHARE_DIR}/mime"
else
 echo "Warning: MIME database not updated. update-mime-database not found."
fi

if [ "$INST_SCOPE" == "M" ]; then
  chmod -R go+r-w "${FS_HOME}"
fi
if [ "$INST_SCOPE" == "U" ]; then
  chmod -R go-r-w-x "${FS_HOME}"
  chmod -R u+r-w "${FS_HOME}"
fi

echo "Done"
echo "FreeShip is installed into $FS_HOME"
echo "  to uninstall enter into $FS_HOME and execute uninstall-FreeShip.sh"


