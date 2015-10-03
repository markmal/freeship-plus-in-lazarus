#shows all packages required for FreeShip binary

RL=
if [ "$1" = "-a" ]; 
 then 
  for L in $(ldd FreeShip|grep -v 'linux-vdso.so.1'|cut -f2|awk '{print $1}'); do RL="$RL $L"; done
 else
  for L in $(ldd FreeShip|grep '=> not found'|cut -f2|awk '{print $1}'); do RL="$RL $L"; done
  if [ "$RL" = "" ] ; then echo "All required packages are installed"; exit 0; fi
fi



if [ -x "$(which dpkg-query 2>/dev/null)" ]; then
  RP=$(dpkg-query -S $RL |cut -d: -f1|sort -u)
  if [ "$1" = "-a" ]; 
    then 
	echo "All packages that are required"
	dpkg-query -l $RP
    else 
	echo $RP
	echo "Required packages that are not installed"
	dpkg-query -l $RP | grep -v '^ii' ;
  fi
fi

if [ -x "$(which yum 2>/dev/null)" ]; then
  yum whatprovides $RL
fi

if [ -x "$(which zypper 2>/dev/null)" ]; then
  for L in $RL; do zypper wp $RL ; done
fi