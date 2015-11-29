# make all sizes icons from one SVG file passed as first parameter
image=$(basename $1 .svg)
chkext=$(basename $1)
# check extension
if [ "${image}.svg" != "${chkext}" ]; then
  echo "Usage: $0 <file.svg>";
  exit 1
fi

for RES in 16 24 32 48 64 96 128; do
    echo convert -density 1200 -trim -resize ${RES}x${RES} -gravity center -background transparent -extent ${RES}x${RES}  $1 PNG32:${RES}/$image.png
    convert -density 1200 -trim -resize ${RES}x${RES} -gravity center -background transparent -extent ${RES}x${RES}  $1 PNG32:${RES}/$image.png
done
