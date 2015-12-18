VER=$(grep ^Version: freeship/DEBIAN/control | sed 's/Version: //')
ARCH=$(grep ^Architecture: freeship/DEBIAN/control | sed 's/Architecture: //')
fakeroot dpkg-deb --build freeship
mv freeship.deb freeship_${VER}-all_${ARCH}.deb
lintian freeship_${VER}-all_${ARCH}.deb |tee lintian.out