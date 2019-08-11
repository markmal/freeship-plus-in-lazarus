function printHelp {
echo "Shows packages required for FreeShip executable"
echo "Usage: $0 [-a][-n] [-f <filename>]"
echo " -a 		show all packages that the executable depends on including"
echo "    		recursive dependencies. Otherwise it shows only immediate"
echo "    		dependency packages."
echo " -n 		show packages that are not installed"
echo " -f <FileName>	an executable file. Be careful to run this with -a on unknown"
echo "              	files. A virus can be invoked by ldd."
echo "              	By default file is FreeShip in local directory."
echo " -h|-?		Help"
}

ALL=0; NOI=0; FREESHIP=FreeShip
while getopts “anf:” opt; do
  #echo "opt:$opt - arg:$OPTARG"
  case $opt in
    a) ALL=1 ;;
    n) NOI=1 ;;
    f) FREESHIP=$OPTARG ;;
    h) printHelp; exit ;;
    ?) printHelp; exit ;;
    *) printHelp; exit ;;
  esac
done

echo "Executable: $FREESHIP"

ARCH=$(objdump -p $FREESHIP | grep 'file format'|sed 's/.*file format elf..-//')
if [ "$ARCH" == "x86-64" ]; then ARCH=amd64; fi
echo "Architecture: $ARCH"

# get list of required libs
RL=
if [ $ALL -eq 1 ]; 
 then 
  for L in $(ldd $FREESHIP | grep -v 'linux-vdso.so.1'|cut -f2|awk '{print $1}'); do RL="$RL $L"; done
  #echo Libs: $RL
 else
  for L in $(objdump -p $FREESHIP|grep " NEEDED " | awk '{print $2}'); do RL="$RL $L"; done
  #echo Libs: $RL
fi

if [ -x "$(which dpkg-query 2>/dev/null)" ]; then
  RP=$(dpkg-query -S $RL | grep ":${ARCH}:" | cut -d: -f1,2|sort -u)
  echo Required Packages: $RP

  if [ $NOI -eq 1 ]; then
	echo "Required packages that are not installed: "
	dpkg-query -l $RP | grep ":$ARCH" | grep -v '^ii' | awk '{print $2}';
  fi
fi


if [ -x "$(which yum 2>/dev/null)" ]; then
  yum whatprovides $RL
fi

if [ -x "$(which zypper 2>/dev/null)" ]; then
  for L in $RL; do zypper wp $RL ; done
fi