prefix ?= /usr
FS_HOME = $(prefix)/share/FreeShip
FS_APP = $(FS_HOME)

all: build build-utils

.PHONY: all

ICON_SIZES = 16 24 32 48 256

clean:
	find . -type f -iname "*.o" | xargs -iFILE rm FILE
	rm -f FreeShip
	rm -f FreeShip.dbg
	rm -f freeship.xpm
	rm -f freeship-*.png

distclean: clean

build: build-icons
	lazbuild -B -r --widgetset=qt FreeShip.lpi

build-win32:
	lazbuild -B -r --widgetset=qt --os=win32 --cpu=i386 FreeShip.lpi

build-utils: versinfo

build-icons:
	for i in $(ICON_SIZES); do inkscape --export-png=freeship-$$i.png -w $$i -h $$i FreeShip_icon.svg ; done
	convert freeship-32.png freeship.xpm

versinfo:
	lazbuild -B Utils/versinfo.lpi

install:
	# ./install-system.sh
	install -D -s FreeShip $(DESTDIR)/$(prefix)/bin/FreeShip
	mkdir -p $(DESTDIR)/$(FS_HOME)/Import
	mkdir -p $(DESTDIR)/$(FS_HOME)/Ships

	cp -r Languages $(DESTDIR)/$(FS_APP)/
	# cp -r Manuals $(DESTDIR)/$(FS_APP)/
	# cp -r Ships $(DESTDIR)/$(FS_APP)/

	# install -D -b FreeShip.ini $(DESTDIR)/$(prefix)/etc/FreeShip.ini

	install -D freeship.desktop $(DESTDIR)/$(prefix)/share/applications/freeship.desktop
	install -D freeship.xpm $(DESTDIR)/$(prefix)/share/pixmaps/freeship.xpm


uninstall:
	# ./uninstall-system.sh

release:
	# ./release.sh

zipbin:
	./zipbin.sh
