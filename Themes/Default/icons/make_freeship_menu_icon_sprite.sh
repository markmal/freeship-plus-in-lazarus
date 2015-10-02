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

# Script should be placed in for example /themes folder. 
# Then, for example, if you want to convert all svg files in folder /themes/midnight-blue/svg There you can run it with:
#     ./make_freeship_menu_icon_sprite ./midnight-blue



# We will process the current folder each image in alphabetical order:  trim, then resize and then add uniform border

echo "Create png from each svg & copying png to png folder..."

THEME=$1

for RES in 16 24 32 48 64 128
do
	COUNTER=0
	
	for filename in `ls THEME/svg/*.svg | sort -V`; 
	do
	 	image="${filename%.*}"
		echo "$COUNTER Converting $image to ${RES}x${RES}..."
		mkdir -p ./${RES}
		convert -density 1200 -trim -resize ${RES}x${RES} -gravity center -background transparent -extent ${RES}x${RES}  $image.svg PNG32:./${THEME}/${RES}/$image.png
		# echo "Apply drop shadow"
		# convert icon_set_hor.png  \( +clone -background black -shadow 50x4+1+1 -channel A -level 0,50% +channel \) -compose DstOver -gravity center -composite icon_sprite_set_uc.png


		COUNTER=$((COUNTER+1))

	done
	
	echo "Creating horizontal ${RES} sprite..."
	# remove old sprite to avoid appending forever
	rm ./${RES}/icon-sprite-${RES}.png
	# make new sprite
	convert +append ./${THEME}/${RES}/*.png PNG32:./${THEME}/${RES}/icon-sprite-${RES}.png
	# echo "Compressing final icon"
	# convert -quality 0 +dither ./${RES}/icon-sprite-${RES}.png PNG32:./${RES}/icon-sprite-${RES}.png

done

echo "Creating preview image with dark background"
# +repage needed to keep image same size
convert -background "#444444" -flatten +repage -border 10x10 -bordercolor "#444444" ./24/icon-sprite-24.png preview/sprite_preview_dark.png

echo "Creating preview image with white background"
convert -background "#ffffff" -flatten +repage -border 10x10 -bordercolor "#ffffff" ./24/icon-sprite-24.png preview/sprite_preview_white.png

echo "Ready, $COUNTER .png images converted. "


