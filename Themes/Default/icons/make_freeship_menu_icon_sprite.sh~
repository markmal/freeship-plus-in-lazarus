#!/bin/bash

# place in the directory where we will have multiple *.svg files
# this script will loop over all the PNG files in the current folder and  use Imagemagick "convert" :
#   - convert .svg to a .png
#   - autocrop / trim
#   - resize to defined values
#   - add a margin around the image
#   - append horizontally
#   - add drop shadow

## INSTALL to use this script
##   sudo apt-get install imagemagick

# We will process the current folder each image in alphabetical order:  trim, then resize and then add uniform border

echo "Create png from each svg & copying png to png folder..."

CURRENTFOLDER=${PWD##*/}

for RES in 16 24 32 48 64 128
do
	COUNTER=0
	
	for filename in `ls svg/*.svg | sort -V`; 
	do
	 	image="${filename%.*}"
		echo "$COUNTER Converting $image to ${RES}x${RES}..."
		convert -density 1200 -trim -resize ${RES}x${RES} -gravity center -background transparent -extent ${RES}x${RES}  $image.svg PNG32:$image.png
		# dropshadow
		# convert $image.png  \( +clone -background white -shadow 50x4+3+3 -channel A -level 0,10% +channel \) -compose DstOver -gravity center -composite $image.png

		mv $image.png ./${RES}
	
		COUNTER=$((COUNTER+1))

	done
	
	echo "Creating horizontal ${RES} sprite..."
	# remove old sprite to avoid appending forever
	rm ./${RES}/icon-sprite-${RES}.png
	# make new sprite
	convert +append ./${RES}/*.png PNG32:./${RES}/icon-sprite-${RES}.png
	# echo "Compressing final icon"
	# convert -quality 0 +dither icon_sprite_set_uc.png PNG32:${CURRENTFOLDER}_sprite.png

done

echo "Creating preview image with dark background"
# +repage needed to keep image same size
convert -background "#444444" -flatten +repage -border 10x10 -bordercolor "#444444" ./24/icon-sprite-24.png preview/${CURRENTFOLDER}_sprite_preview_dark.png

echo "Creating preview image with white background"
convert -background "#ffffff" -flatten +repage -border 10x10 -bordercolor "#ffffff" ./24/icon-sprite-24.png preview/${CURRENTFOLDER}_sprite_preview_white.png

echo "Ready, $COUNTER .png images converted. "


