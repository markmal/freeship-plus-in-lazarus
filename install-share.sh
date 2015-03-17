# Installs FreeShip into shared directories. You should be able to sudo

FS_HOME=${HOME}/FreeShip

mkdir -p ${FS_HOME}/Export
mkdir -p ${FS_HOME}/Import
mkdir -p ${FS_HOME}/Ships
mkdir -p ${FS_HOME}/Temp

FS_APP=${/usr/share/FreeShip}

sudo mkdir -p ${FS_APP}/Exec
sudo mkdir -p ${FS_APP}/Languages
sudo mkdir -p ${FS_APP}/Manuals
sudo mkdir -p ${FS_APP}/Ships

sudo cp -r Languages ${FS_APP}/Languages

mkdir ${HOME}/.config/FreeShip
CFG=${HOME}/.config/FreeShip/FreeShip.ini

if [ -f $CFG ] && exit

echo "[Directories]" >$CFG
echo "LanguagesDirectory=${FS_APP}/Languages" >>$CFG
echo "ExecDirectory=${FS_APP}/Exec" >>$CFG
echo "ManualsDirectory=${FS_APP}/Manuals" >>$CFG
echo "TempDirectory=${FS_HOME}/Temp" >>$CFG
echo "OpenDirectory=${FS_APP}/Ships" >>$CFG
echo "SaveDirectory=${FS_HOME}/Ships" >>$CFG
echo "ImportDirectory=${FS_APP}/Import" >>$CFG
echo "ExportDirectory=${FS_HOME}/Export" >>$CFG

sudo mv FreeShip /usr/bin/








