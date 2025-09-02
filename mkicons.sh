cd icons
for D in 24 32 48 64 96 128; do [ ! -d $D ] && mkdir $D; done

PS=16
for S in 24 32 48 64 96 128; do
  for I in ${PS}/*.png; do
    F=$(basename $I)
    convert $I -adaptive-resize ${S}x${S} $S/$F
  done
  PS=$S
done
cd ..

# create Windows icon from all sizes
convert $(find Themes -name 00-freeship.png) 00-freeship.ico
# you should manually load it in Project Options