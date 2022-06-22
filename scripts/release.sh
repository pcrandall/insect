#!/bin/bash
# Generates cross builds for all supported platforms.
#
# This script is used to build binaries for all supported platforms. Cgo is
# disabled to make sure binaries are statically linked. Appropriate flags are
# given to the go compiler to strip binaries. Current git tag is passed to the
# compiler by default to be used as the version in binaries. These are then
# compressed in an archive form (`.zip` for windows and `.tar.gz` for the rest)
# within a folder named `dist`.

set -o verbose

[ -z $version ] && version=$(git describe --tags)

buildDir=dist/insect
dist=dist/$version
mkdir -p $dist
cp ./README.md $buildDir/README.md;

# MacOs
cp $buildDir/insect-mac $buildDir/insect; \
tar czf $dist/insect-$version-darwin.tar.gz $buildDir/{insect,res.neu,WebView2Loader.dll,README.md}; \
file=$dist/insect-$version-darwin.tar.gz; \
md5sum $file > $file.md5;

# Linux
cp $buildDir/insect-linux $buildDir/insect; \
tar czf $dist/insect-$version-linux.tar.gz $buildDir/{insect,res.neu,WebView2Loader.dll,README.md}; \
file=$dist/insect-$version-linux.tar.gz; \
md5sum $file > $file.md5;

# Windows
cp $buildDir/insect-win.exe $buildDir/insect.exe; \
cp ./scripts/windowsInstall.bat $buildDir ; \
cp ./resources/icons/favicon.ico $buildDir; \
zip $dist/insect-$version-windows.zip $buildDir/{insect.exe,res.neu,WebView2Loader.dll,README.md,windowsInstall.bat,favicon.ico}; \
file=$dist/insect-$version-windows.zip; \
md5sum $file > $file.md5;

rm $buildDir/insect;
rm $buildDir/insect.exe;
rm $buildDir/README.md;
